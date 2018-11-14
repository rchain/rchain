package coop.rchain.casper.util.rholang

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.casper.genesis.contracts.TestSetUtil
import monix.execution.Scheduler
import monix.eval.Task
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccounting}
import coop.rchain.shared.PathOps.RichPath
import java.nio.file.{Files, Path, Paths}
import coop.rchain.rholang.interpreter.{PrettyPrinter, Runtime}
import coop.rchain.rspace.Checkpoint
import coop.rchain.shared.StoreType.InMem
import coop.rchain.models._
import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.catscontrib.TaskContrib._
import scala.collection.mutable

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
class Interactive private (runtime: Runtime)(implicit scheduler: Scheduler) {
  private implicit val rand = Blake2b512Random(128)

  private val prettyPrinter = PrettyPrinter()

  private val checkpoints = new mutable.HashMap[String, Checkpoint]()
  checkpoints.update("empty", runtime.space.createCheckpoint().unsafeRunSync)

  def checkpointNames: List[String] = checkpoints.keys.toList

  def tuplespace: String = StoragePrinter.prettyPrint(runtime.space.store)

  def eval(code: String): Unit = {
    TestSetUtil.eval(code, runtime)
    val errors = runtime.errorLog.readAndClearErrorVector()
    if (errors.nonEmpty) {
      println("Errors during execution:")
      errors.foreach(println)
    }
  }
  def evalFile(path: String): Unit = eval(scala.io.Source.fromFile(path).mkString)
  def query(code: String, name: String = "__out__"): Seq[Par] = {
    checkpoint("preQuery")
    eval(code)
    val result = runtime.space
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
    runtime.close().unsafeRunSync

  def checkpoint(name: String): Unit =
    checkpoints.update(name, runtime.space.createCheckpoint().unsafeRunSync)
  def getCheckpoint(name: String): Option[Checkpoint] = checkpoints.get(name)

  def restore(name: String): Boolean =
    checkpoints
      .get(name)
      .fold(false)(ch => {
        runtime.space.reset(ch.root).unsafeRunSync
        true
      })
}
object Interactive {
  def apply(): Interactive = {
    implicit val scheduler = Scheduler.io("rhoang-interpreter")

    new Interactive(TestSetUtil.runtime)
  }
}
