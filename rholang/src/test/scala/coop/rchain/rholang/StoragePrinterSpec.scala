package coop.rchain.rholang

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.DeployData
import coop.rchain.metrics
import coop.rchain.metrics.Span.TraceId
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources.mkRuntime
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.interpreter.{Interpreter, InterpreterUtil, NormalizerEnv}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class StoragePrinterSpec extends FlatSpec with Matchers {
  implicit val traceId: TraceId = Span.empty
  private val mapSize           = 10L * 1024L * 1024L
  private val tmpPrefix         = "rspace-store-"
  private val maxDuration       = 5.seconds

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
              NormalizerEnv.Empty
            )
          }
          pretty <- StoragePrinter.prettyPrintUnmatchedSends(runtime.space)
          _      = assert(pretty == "@{1}!(Nil)")
        } yield ()
      }
      .runSyncUnsafe(maxDuration)
  }

  it should "print unmatched sends of multiple deploys" in {
    def mkSig(n: Int) = ByteString.copyFrom(Array[Byte](n.toByte))
    mkRuntime[Task](tmpPrefix, mapSize)
      .use { runtime =>
        val deploy1 = DeployData(term = "@1!(Nil)", sig = mkSig(1))
        val deploy2 = DeployData(term = "@2!(Nil)", sig = mkSig(2))
        val deploy3 = DeployData(term = "@3!(Nil) | for(_ <- @3) { Nil }", sig = mkSig(3))
        for {
          unmatchedSends <- StoragePrinter.prettyPrintUnmatchedSends(
                             List(deploy1, deploy2, deploy3),
                             runtime
                           )
          result = """Deploy 01:
              |@{1}!(Nil)
              |
              |Deploy 02:
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
          deploy         = DeployData(term = "@1!(Nil) | @2!(Nil)")
          unmatchedSends <- StoragePrinter.prettyPrintUnmatchedSends(deploy, runtime)
          _              = assert(unmatchedSends == "@{2}!(Nil)")
        } yield ()
      }
      .runSyncUnsafe(maxDuration)
  }
}
