package coop.rchain.state

import java.nio.file.{Path, Paths}

import cats.syntax.all._
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest._

class TrieExporterSpec extends FlatSpec with Matchers {

  implicit val metrics: Metrics[Task] = new Metrics.MetricsNOP[Task]()
  implicit val span: Span[Task]       = NoopSpan[Task]()
  implicit val log: Log[Task]         = new Log.NOPLog[Task]()

  private def createExporterWithStores(dir: Path) =
    for {
      rspace              <- Runtime.setupRSpace[Task](dir, 1073741824L)
      (_, _, historyRepo) = rspace
      exporter            <- historyRepo.exporter
    } yield exporter

  val projParentDir  = Paths.get("../").toAbsolutePath
  val testDataDir    = projParentDir.resolve("rchain-test-data")
  val testnetHistory = "testnet-rspace/casper"
  val mainNetHistory = "mainnet_node_state.27.03.2020/rspace/casper"

  val pathSource  = testDataDir.resolve(testnetHistory)
  val pathRestore = testDataDir.resolve("casper-restore")

  println(s"SOURCE  DIR: ${pathSource}")
  println(s"RESTORE DIR: ${pathRestore}")

  "export all" should "be correct" in {
    // Import extension methods (syntax) for exporter
    import coop.rchain.rspace.state.syntax._

    val doExport = for {
      exporter <- createExporterWithStores(pathSource)
      root     <- exporter.getRoot
      _        = println(s"EXPORTER ROOT: ${root}")
      _        <- exporter.writeToDisk(root, pathRestore, chunkSize = 3000)
    } yield ()

    doExport.runSyncUnsafe()
  }

}
