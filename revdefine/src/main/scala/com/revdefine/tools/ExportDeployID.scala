package com.revdefine.tools

import coop.rchain.blockstorage.KeyValueBlockStore.bytesToBlockProto
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.syntax._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.lmdbjava.ByteBufferProxy.PROXY_SAFE
import org.lmdbjava._
import scodec.bits.ByteVector

import java.io.PrintWriter
import java.nio.file.Path
import scala.collection.JavaConverters._
object ExportDeployID {
  def main(args: Array[String]): Unit = {
    val lmdbPath = Path.of("/rchain/mainnet/obs-prehf1/rnode/blockstorage")
    val flags    = Seq(EnvFlags.MDB_NOTLS, EnvFlags.MDB_NORDAHEAD)
    val env = Env
      .create(PROXY_SAFE)
      .setMapSize(1024L * 1024L * 1024L * 1024L)
      .setMaxDbs(20)
      // Maximum parallel readers
      .setMaxReaders(2048)
      .open(lmdbPath.toFile, flags: _*)
    val dbi  = env.openDbi("blocks")
    val path = Path.of("/rchain/deployId.txt")
    val file = path.toFile
    val bw   = new PrintWriter(file)

    val txn      = env.txnRead()
    val iterator = dbi.iterate(txn)
    val iter     = iterator.iterator.asScala.map(c => (c.key, c.`val`))
    iter.foreach {
      case (_, v) =>
        val value = bytesToBlockProto[Task](ByteVector(v).toArray)
          .map(BlockMessage.from)
          .runSyncUnsafe()
          .right
          .get
        value.body.deploys.foreach(
          d =>
            bw.write(
              s"${d.deploy.sig.toHexString},${value.blockHash.toHexString}\n"
            )
        )
    }
  }
}
