package coop.rchain.node

import coop.rchain.blockstorage.{FileLMDBIndexBlockStore, KeyValueBlockStore}
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.lmdb.Context
import coop.rchain.metrics.Metrics
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import monix.eval.Task
import org.rogach.scallop.ScallopConf
import monix.execution.Scheduler.Implicits.global

import java.nio.file.Path

final case class Options(arguments: Seq[String]) extends ScallopConf(arguments) {
  val width = 120
  helpWidth(width)
  printedName = "migrate-blockstore"

  val originalBlockStore = opt[Path](
    descr = "original block store directory",
    required = true
  )

  val targetDataDir = opt[Path](
    descr = "Output block store directory",
    required = true
  )

  verify()

}
object BlockStoreMigrate {
  def main(args: Array[String]): Unit = {
    val options            = Options(args)
    val originalBlockStore = options.originalBlockStore()
    val targetDataDir      = options.targetDataDir()
    implicit val log       = effects.log
    implicit val metrics   = new Metrics.MetricsNOP[Task]

    val t = for {
      casperStoreManager <- RNodeKeyValueStoreManager[Task](targetDataDir)
      blockKVStore       <- KeyValueBlockStore[Task](casperStoreManager)

      blockFileStorage <- FileLMDBIndexBlockStore.create[Task](originalBlockStore)
      _                <- log.info("Migrating file based block store to key value store.")
      dataStream       <- blockFileStorage.asInstanceOf[FileLMDBIndexBlockStore[Task]].iterateStream
      operation        = dataStream.parEvalMapUnorderedProcBounded(b => blockKVStore.put(b))
      _                <- operation.compile.drain
      _                <- log.info("Migrating file based block store to key value store done.")
      approvedBlock    <- blockFileStorage.getApprovedBlock.map(_.get)
      _                <- blockKVStore.putApprovedBlock(approvedBlock)
      _                <- log.info("Migration on approved block is done.")

    } yield ()
    t.runSyncUnsafe()
  }
}
