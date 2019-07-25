package coop.rchain.casper

import java.nio.file.{Files, Path}

import coop.rchain.blockstorage.util.io.IOError
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.shared.PathOps.RichPath
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.p2p.EffectsTestInstances.{LogStub, LogicalTime}
import coop.rchain.shared.Time
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

class RholangBuildTest extends FlatSpec with Matchers {

  import monix.execution.Scheduler.Implicits.global

  implicit val timeEff: Time[Task]              = new LogicalTime[Task]
  implicit val log                              = new LogStub[Task]
  implicit val metricsEff: Metrics[Task]        = new metrics.Metrics.MetricsNOP[Task]
  implicit val span: Span[Task]                 = NoopSpan[Task]()
  implicit val raiseIOError: RaiseIOError[Task] = IOError.raiseIOErrorThroughSync[Task]

  val genesisPath: Path =
    Path.of("/Users/josephdenman/IdeaProjects/rchain/casper/src/test/scala/coop/rchain/casper")
  val storagePath: Path = Files.createTempDirectory(s"rholang-build-test-")
  val storageSize: Long = 3024L * 1024

  for {
    activeRuntime  <- Runtime.createWithEmptyCost[Task](storagePath, storageSize)
    runtimeManager <- RuntimeManager.fromRuntime[Task](activeRuntime)
    genesisBlock <- {
      implicit val rm: RuntimeManager[Task] = runtimeManager
      Genesis.fromInputFiles[Task](None, 1, ???, None, 0L, Long.MaxValue, "rchain", None)
    }
    _ <- Task.delay(storagePath.recursivelyDelete())
    _ <- Task.delay(genesisPath.resolve("bonds.txt").recursivelyDelete())
    _ <- Task.delay(
          genesisBlock.body.get.state.get.bonds
            .map(_.validator)
            .map(byteKey => Base16.encode(byteKey.toByteArray))
            .foreach { pk =>
              genesisPath.resolve(s"$pk.sk").recursivelyDelete()
            }
        )
  } yield ()

}
