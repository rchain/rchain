package coop.rchain.rholang

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{DeployData, DeployDataProto}
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.{NormalizerEnv, Par}
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{Interpreter, InterpreterUtil}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}
import cats.syntax._
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Secp256k1, Signed}

import scala.concurrent.duration._

class StoragePrinterSpec extends FlatSpec with Matchers {
  private val mapSize     = 10L * 1024L * 1024L
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
    mkRuntime[Task](tmpPrefix, mapSize)
      .use { runtime =>
        for {
          _ <- {
            implicit val c = runtime.cost
            Interpreter[Task].evaluate(
              runtime,
              "@1!(Nil) | @2!(Nil) | for(_ <- @2) { Nil }",
              Map.empty[String, Par]
            )
          }
          pretty <- StoragePrinter.prettyPrintUnmatchedSends(runtime.space)
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
    mkRuntime[Task](tmpPrefix, mapSize)
      .use { runtime =>
        val deploy1 = "@1!(Nil)"
        val deploy2 = "@2!(Nil)"
        val deploy3 = "@3!(Nil) | for(_ <- @3) { Nil }"
        for {
          unmatchedSends <- StoragePrinter.prettyPrintUnmatchedSends(
                             List(deploy1, deploy2, deploy3).map(mkDeploy),
                             runtime
                           )
          result = """Deploy 304502210083d7a25b6157ef5b51cb49adbb314acab716181775e766969c60a9d15171e18b02203e5d244553a9ad39bb3de39672d5db9c3870c1db213498f9695bc1c03f6bd36e:
              |@{1}!(Nil)
              |
              |Deploy 3045022100fafb2654342fd824fe91ba7c5679da99190002a863f15afaf9c0a4b4345b297902200563bb6581dde86d06573332975afdfcfa9b0e76453b5758a10df9b6bace720d:
              |@{2}!(Nil)""".stripMargin
          _      = assert(unmatchedSends == result)
        } yield ()
      }
      .runSyncUnsafe(maxDuration)
  }

  it should "not print unmatched sends from previous deploys" in {
    mkRuntime[Task](tmpPrefix, mapSize)
      .use { runtime =>
        for {
          _ <- {
            implicit val c = runtime.cost
            InterpreterUtil.evaluate[Task](runtime, "@0!(Nil) | for(_ <- @1) { Nil }")
          }
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
