package coop.rchain.node
import coop.rchain.crypto.codec.Base16
import org.lmdbjava.ByteBufferProxy.PROXY_SAFE
import org.lmdbjava._
import scodec.bits.ByteVector

import java.io.PrintWriter
import java.nio.file.Path
import scala.collection.JavaConverters._
object StateHash {
  def main(args: Array[String]): Unit = {
    val lmdbPath = Path.of("/rchain/node29/rnode/rspace/history")
    val flags    = Seq(EnvFlags.MDB_NOTLS, EnvFlags.MDB_NORDAHEAD)
    val env = Env
      .create(PROXY_SAFE)
      .setMapSize(1024L * 1024L * 1024L * 1024L)
      .setMaxDbs(20)
      // Maximum parallel readers
      .setMaxReaders(2048)
      .open(lmdbPath.toFile, flags: _*)
    val dbi  = env.openDbi("rspace-roots")
    val path = Path.of("/rchain/state.txt")
    val file = path.toFile
    val bw   = new PrintWriter(file)

    val txn      = env.txnRead()
    val iterator = dbi.iterate(txn)
    val iter     = iterator.iterator.asScala.map(c => (c.key, c.`val`))
    iter.foreach {
      case (k, v) => bw.write(s"${Base16.encode(ByteVector(k).toArray)},${Base16.encode(ByteVector(v).toArray)}\n")
    }
  }

}
