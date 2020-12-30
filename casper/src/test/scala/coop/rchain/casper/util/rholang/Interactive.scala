package coop.rchain.casper.util.rholang

import java.nio.file.Files

import coop.rchain.casper.genesis.contracts.TestUtil
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Expr.ExprInstance.GString
import coop.rchain.models._
import coop.rchain.rholang.interpreter.{PrettyPrinter, RhoRuntime}
import coop.rchain.rspace.Checkpoint
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager
import monix.eval.Task
import monix.execution.Scheduler

import scala.collection.mutable
import scala.concurrent.duration._

/**
  * This is a really handy class for working interactively with
  * Rholang at the Scala console.
  *
  * {{{
  * >>> import coop.rchain.casper.util.rholang.Interactive
  * >>> val itp = Interactive()
  * >>> itp.eval(""" @0!(0) """)
  * >>> itp.checkpoint("send-zero")
  * >>> itp.tuplespace.split('|').filter(_.contains("!")).head.trim
  * "@{0}!(0)"
  * >>> itp.restore("empty") //empty checkpoint exists by default
  * true
  * >>> itp.tuplespace.split('|').exists(_.contains("!"))
  * false
  * >>> itp.restore("send-zero")
  * true
  * >>> itp.tuplespace.split('|').filter(_.contains("!")).head.trim
  * "@{0}!(0)"
  * >>> itp.cleanUp()
  * }}}
  */
class Interactive private (runtime: RhoRuntime[Task])(implicit scheduler: Scheduler) {
  implicit private val rand = Blake2b512Random(128)

  private val prettyPrinter = PrettyPrinter()

  private val checkpoints = new mutable.HashMap[String, Checkpoint]()
  checkpoints.update("empty", runtime.createCheckpoint.unsafeRunSync)

  def checkpointNames: List[String] = checkpoints.keys.toList

  def eval(code: String): Unit =
    TestUtil.eval(code, runtime, Map.empty).runSyncUnsafe(Duration.Inf)
  def evalFile(path: String): Unit = eval(scala.io.Source.fromFile(path).mkString)
  def query(code: String, name: String = "__out__"): Seq[Par] = {
    checkpoint("preQuery")
    eval(code)
    val result = runtime
      .getData(
        Par().copy(exprs = Seq(Expr(GString(name))))
      )
      .unsafeRunSync
    restore("preQuery")
    checkpoints.remove("preQuery")

    result.flatMap(_.a.pars)
  }
  def pp(term: Par): String = prettyPrinter.buildString(term)

  def cleanUp(): Unit =
    runtime.close.unsafeRunSync

  def checkpoint(name: String): Unit =
    checkpoints.update(name, runtime.createCheckpoint.unsafeRunSync)
  def getCheckpoint(name: String): Option[Checkpoint] = checkpoints.get(name)

  def restore(name: String): Boolean =
    checkpoints
      .get(name)
      .fold(false)(ch => {
        runtime.reset(ch.root).unsafeRunSync
        true
      })
}
object Interactive {
  def apply(): Interactive = {
    implicit val scheduler                 = Scheduler.io("rholang-interpreter")
    implicit val logger: Log[Task]         = Log.log[Task]
    implicit val metricsEff: Metrics[Task] = new Metrics.MetricsNOP[Task]
    implicit val noopSpan: Span[Task]      = NoopSpan[Task]()
    implicit val kvsManager                = InMemoryStoreManager[Task]

    val space = RhoRuntime
      .setupRhoRSpace[Task](
        Files.createTempDirectory("interactive-"),
        1024 * 1024L * 1024L,
        kvsManager
      )
      .unsafeRunSync
    new Interactive(RhoRuntime.createRhoRuntime[Task](space).runSyncUnsafe(5.seconds))
  }
}
