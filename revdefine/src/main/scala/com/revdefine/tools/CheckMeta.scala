package com.revdefine.tools

import coop.rchain.blockstorage.dag.BlockDagKeyValueStorage
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.Metrics
import coop.rchain.shared.Log
import monix.eval.Task
import org.rogach.scallop.ScallopConf
import monix.execution.Scheduler.Implicits.global

import java.nio.file.Path

final case class CheckMetaOptions(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "replay"

  val dataDir = opt[Path](
    descr = s"RNode data dir.",
    required = true
  )

  verify()
}

object CheckMeta {
  def main(args: Array[String]): Unit = {
    val options = CheckMetaOptions(args)
    val dataDir = options.dataDir()

    implicit val log: Log[Task]                    = Log.log
    implicit val metrics: Metrics.MetricsNOP[Task] = new Metrics.MetricsNOP[Task]()

    (for {
      rnodeStoreManager <- RNodeKeyValueStoreManager[Task](dataDir)
      blockDagStorage   <- BlockDagKeyValueStorage.create[Task](rnodeStoreManager)

      rep    <- blockDagStorage.getRepresentation
      latest = rep.lastFinalizedBlock
      _      <- log.info(s"Last finalized block is ${Base16.encode(latest.toByteArray)}")
    } yield ()).runSyncUnsafe()
  }
}
