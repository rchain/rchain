package coop.rchain.rholang

import cats.effect.IO
import coop.rchain.casper.protocol.DeployData
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.syntax._
import coop.rchain.shared.{Base16, Log}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.unsafe.implicits.global

class StoragePrinterSpec extends AnyFlatSpec with Matchers {
  private val tmpPrefix = "rspace-store-"
  private val deployerSk = PrivateKey(
    Base16.unsafeDecode("17f242c34491ff8187ec94ec1508fed8b487b872f2bb97b437f4d4e44345cee6")
  )
  private val SHARD_ID = "root-shard"

  implicit val logF: Log[IO]            = new Log.NOPLog[IO]
  implicit val noopMetrics: Metrics[IO] = new metrics.Metrics.MetricsNOP[IO]
  implicit val noopSpan: Span[IO]       = NoopSpan[IO]()

  behavior of "StoragePrinter.prettyPrintUnmatchedSends"

  it should "print unmatched sends" in {
    mkRuntime[IO](tmpPrefix)
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
      .unsafeRunSync()
  }

  private def mkDeploy(term: String) =
    Signed(
      DeployData(
        timestamp = 0,
        phloPrice = 0,
        phloLimit = 0,
        validAfterBlockNumber = 0,
        term = term,
        shardId = SHARD_ID
      ),
      Secp256k1,
      deployerSk
    )

  it should "print unmatched sends of multiple deploys" in {
    mkRuntime[IO](tmpPrefix)
      .use { runtime =>
        val deploy1 = "@1!(Nil)"
        val deploy2 = "@2!(Nil)"
        val deploy3 = "@3!(Nil) | for(_ <- @3) { Nil }"
        for {
          unmatchedSends <- StoragePrinter.prettyPrintUnmatchedSends(
                             List(deploy1, deploy2, deploy3).map(mkDeploy),
                             runtime
                           )
          result = """Deploy 304302205f0ca86c04a9614b474372e38e3d92c173690ffb864989f555720e004e96da72021f1f2243e6d33e5cd444f5321533b1afb52bfe830fc2f78aab77e0f7e6130669:
              |@{1}!(Nil)
              |
              |Deploy 3045022100c126afae3a9c135ab08812bafb6fe54353f80f77c7b628b93a738891518197b5022031e220e1a3d4b859752ebe80153148223a0d75308c8b49bf6c922c5dd51129a1:
              |@{2}!(Nil)""".stripMargin
          _      = assert(unmatchedSends == result)
        } yield ()
      }
      .unsafeRunSync()
  }

  it should "not print unmatched sends from previous deploys" in {
    mkRuntime[IO](tmpPrefix)
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
      .unsafeRunSync()
  }
}
