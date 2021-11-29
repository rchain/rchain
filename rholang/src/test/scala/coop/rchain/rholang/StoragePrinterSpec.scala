package coop.rchain.rholang

import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.syntax._
import coop.rchain.shared.{Base16, Log}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class StoragePrinterSpec extends FlatSpec with Matchers {
  private val tmpPrefix   = "rspace-store-"
  private val maxDuration = 5.seconds
  private val deployerSk = PrivateKey(
    Base16.unsafeDecode("17f242c34491ff8187ec94ec1508fed8b487b872f2bb97b437f4d4e44345cee6")
  )

  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]       = NoopSpan[Task]()

  behavior of "StoragePrinter.prettyPrintUnmatchedSends"

  it should "print unmatched sends" in {
    mkRuntime[Task](tmpPrefix)
      .use { runtime =>
        for {
          _ <- {
            runtime.evaluate(
              "@1!(Nil) | @2!(Nil) | for(_ <- @2) { Nil }"
            )
          }
          pretty <- StoragePrinter.prettyPrintUnmatchedSends(runtime)
          _      = assert(pretty == "@{1}!(Nil)")
        } yield ()
      }
      .runSyncUnsafe(maxDuration)
  }

  private def mkDeploy(term: String) =
    Signed(
      DeployData(
        timestamp = 0,
        phloPrice = 0,
        phloLimit = 0,
        validAfterBlockNumber = 0,
        term = term
      ),
      Secp256k1,
      deployerSk
    )

  it should "print unmatched sends of multiple deploys" in {
    mkRuntime[Task](tmpPrefix)
      .use { runtime =>
        val deploy1 = "@1!(Nil)"
        val deploy2 = "@2!(Nil)"
        val deploy3 = "@3!(Nil) | for(_ <- @3) { Nil }"
        for {
          unmatchedSends <- StoragePrinter.prettyPrintUnmatchedSends(
                             List(deploy1, deploy2, deploy3).map(mkDeploy),
                             runtime
                           )
          result = """Deploy 30450221008aaf85500ad932038afaea985e21a112dfd9c1cbad0a4888383205f8406300f402206988d6c1613875d6726b39268f2be042d90a7da9a5b0a59c4675c6441b0f73fe:
              |@{1}!(Nil)
              |
              |Deploy 3045022100cc80e7d7cf0582ad9fb650fd481b7c55ff3120cd641e791feb1cad9da919459f0220782c7a8048a6ad8c302d66b4711cf8024bc15746a20fa6b8175a81166dce6807:
              |@{2}!(Nil)""".stripMargin
          _      = assert(unmatchedSends == result)
        } yield ()
      }
      .runSyncUnsafe(maxDuration)
  }

  it should "not print unmatched sends from previous deploys" in {
    mkRuntime[Task](tmpPrefix)
      .use { runtime =>
        for {
          _      <- runtime.evaluate("@0!(Nil) | for(_ <- @1) { Nil }")
          deploy = mkDeploy("@1!(Nil) | @2!(Nil)")
          unmatchedSends <- StoragePrinter.prettyPrintUnmatchedSends(
                             deploy,
                             runtime
                           )
          _ = assert(unmatchedSends == "@{2}!(Nil)")
        } yield ()
      }
      .runSyncUnsafe(maxDuration)
  }
}
