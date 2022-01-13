package com.revdefine.tools

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.syntax._
import coop.rchain.models.syntax._
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.RhoRuntime
import coop.rchain.rholang.interpreter.SystemProcesses.BlockData
import coop.rchain.rspace.syntax._
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.rogach.scallop.ScallopConf

import java.nio.file.Path

final case class ReplayOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "replay"

  val blockHash = opt[String](
    descr = s"blockHash",
    required = true
  )

  val isGenesis = opt[Boolean](
    descr = s"isGenesis",
    required = true
  )
  val dataDir = opt[Path](
    descr = s"RNode data dir.",
    required = true
  )
  verify()
}
object Replay {
  def main(args: Array[String]): Unit = {
    val options   = ReplayOptions(args)
    val dataDir   = options.dataDir()
    val blockHash = options.blockHash()
    val isGenesis = options.isGenesis()

    import coop.rchain.rholang.interpreter.storage._
    implicit val span                                           = NoopSpan[Task]()
    implicit val log: Log[Task]                                 = Log.log
    implicit val metrics                                        = new Metrics.MetricsNOP[Task]()
    implicit val m: Match[Task, BindPattern, ListParWithRandom] = matchListPar[Task]

    val task = for {
      _                 <- log.info(s"${dataDir}, ${blockHash}, ${isGenesis}")
      rnodeStoreManager <- RNodeKeyValueStoreManager[Task](dataDir)
      blockStore        <- KeyValueBlockStore(rnodeStoreManager)
      blockOpt          <- blockStore.get(blockHash.unsafeHexToByteString)
      block             = blockOpt.get
      store             <- rnodeStoreManager.rSpaceStores
      spaces <- RSpace
                 .createWithReplay[Task, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                   store
                 )
      (rSpacePlay, rSpaceReplay) = spaces
      runtimes                   <- RhoRuntime.createRuntimes[Task](rSpacePlay, rSpaceReplay, true, Seq.empty)
      (_, replayRuntime)         = runtimes
      preState                   <- replayRuntime.emptyStateHash
      _                          <- log.info(s"block ${block}")
      stateHash <- replayRuntime.replayComputeState(block.body.state.preStateHash)(
                    block.body.deploys,
                    block.body.systemDeploys,
                    BlockData.fromBlock(block),
                    Map.empty,
                    isGenesis = isGenesis
                  )
      _ <- log.info(s"replay result ${stateHash}")
      _ <- log
            .info(s"replay post hash is ${stateHash.right.get.toHexString}")
            .whenA(stateHash.isRight)
    } yield ()
    task.runSyncUnsafe()
  }
}
