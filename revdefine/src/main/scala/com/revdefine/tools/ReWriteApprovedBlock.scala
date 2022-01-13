package com.revdefine.tools

import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.KeyValueBlockStore
import coop.rchain.casper.protocol.{ApprovedBlock, ApprovedBlockCandidate}
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.models.syntax._
import coop.rchain.metrics.{Metrics, NoopSpan}
import coop.rchain.models.{BindPattern, ListParWithRandom}
import coop.rchain.rspace.{Match}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.rogach.scallop.ScallopConf

import java.nio.file.Path

final case class ReWriteApprovedBlockOptions(arguments: Seq[String])
    extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "replay"

  val blockHash = opt[String](
    descr = s"blockHash",
    required = true
  )

  val dataDir = opt[Path](
    descr = s"RNode data dir.",
    required = true
  )
  verify()
}
object ReWriteApprovedBlock {
  def main(args: Array[String]): Unit = {
    val options   = ReplayOptions(args)
    val dataDir   = options.dataDir()
    val blockHash = options.blockHash()

    import coop.rchain.rholang.interpreter.storage._
    implicit val span                                           = NoopSpan[Task]()
    implicit val log: Log[Task]                                 = Log.log
    implicit val metrics                                        = new Metrics.MetricsNOP[Task]()
    implicit val m: Match[Task, BindPattern, ListParWithRandom] = matchListPar[Task]

    val task = for {
      rnodeStoreManager <- RNodeKeyValueStoreManager[Task](dataDir)
      blockStore        <- KeyValueBlockStore(rnodeStoreManager)
      approvedBlock     <- blockStore.getApprovedBlock
      _                 <- log.info(s"approve block is ${approvedBlock}")
      blockOpt          <- blockStore.get(blockHash.unsafeHexToByteString)
      _                 <- log.info(s"target new approved block is ${blockOpt}")
      block             = blockOpt.get
      newApproved       = ApprovedBlock(ApprovedBlockCandidate(block, 0), List.empty)
      _                 <- log.info(s"new approved ${newApproved}")
      _                 <- blockStore.putApprovedBlock(newApproved)
    } yield ()
    task.runSyncUnsafe()
  }
}
