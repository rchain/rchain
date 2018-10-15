package coop.rchain.rspace

import internal._

import java.nio.ByteBuffer
import java.nio.file.Path

import org.lmdbjava.{Dbi, Env, Txn, TxnOps}
import scodec.Codec
import coop.rchain.shared.ByteVectorOps._
import coop.rchain.shared.PathOps._
import scodec.bits.BitVector
import kamon._
import scala.util.control.NonFatal

trait LMDBOps extends CloseOps {

  protected[rspace] type Transaction = Txn[ByteBuffer]

  private[rspace] def createTxnRead() = {
    failIfClosed()
    env.txnRead
  }

  private[rspace] def createTxnWrite() = {
    failIfClosed()
    env.txnWrite
  }

  protected[this] def databasePath: Path
  protected[this] def env: Env[ByteBuffer]

  private[this] val gaugeTags    = Map("path" -> databasePath.toString)
  private[this] val sizeGauge    = Kamon.gauge("size").refine(gaugeTags)
  private[this] val entriesGauge = Kamon.gauge("entries").refine(gaugeTags)

  protected[this] def updateGauges() = {
    sizeGauge.set(databasePath.folderSize)
    entriesGauge.set(env.stat().entries)
  }

  private[rspace] def withTxn[R](txn: Txn[ByteBuffer])(f: Txn[ByteBuffer] => R): R =
    try {
      val ret: R = f(txn)
      txn.commit()
      ret
    } catch {
      case NonFatal(ex) =>
        ex.printStackTrace
        throw ex
    } finally {
      updateGauges()
      txn.close()
    }

  /** The methods:
    * `def get[V](txn: Txn[ByteBuffer], key: Blake2b256Hash)`
    * `def put[V](txn: Txn[ByteBuffer], key: Blake2b256Hash, data: V)`
    * `def delete(txn: Txn[ByteBuffer], key: Blake2b256Hash): Unit`
    *
    * assume that it is good enough to use the raw `Blake2b256Hash` instead of the scodec encoded `Blake2b256Hash`.
    * This is true in the current execution, but will likely need to be adjusted if the Store & TrieStore move back to abstract keys.
    *
    * It has two gains:
    * - the methods are less complex
    * - the encoding is faster (~1,5x)
    *
    * the original code:
    * `Codec[Blake2b256Hash].encode(key).get.bytes.toDirectByteBuffer`
    *
    * a benchmark showing the difference can be found in the rspaceBench project (KeyBench)
    */
  implicit class RichDbi(val dbi: Dbi[ByteBuffer]) {

    def get[V](txn: Txn[ByteBuffer], key: Blake2b256Hash)(implicit codecV: Codec[V]): Option[V] =
      Option(dbi.get(txn, key.bytes.toDirectByteBuffer))
        .map { bytes =>
          codecV.decode(BitVector(bytes)).map(_.value).get
        }

    def put[V](txn: Txn[ByteBuffer], key: Blake2b256Hash, data: V)(
        implicit codecV: Codec[V]
    ): Unit =
      if (!dbi.put(
            txn,
            key.bytes.toDirectByteBuffer,
            codecV.encode(data).map(_.bytes.toDirectByteBuffer).get
          )) {
        throw new Exception(s"could not persist: $data")
      }

    def delete(txn: Txn[ByteBuffer], key: Blake2b256Hash): Unit =
      if (!dbi.delete(txn, key.bytes.toDirectByteBuffer)) {
        throw new Exception(s"could not delete: $key")
      }

    def get[K, V](
        txn: Txn[ByteBuffer],
        key: K
    )(implicit codecK: Codec[K], codecV: Codec[V]): Option[V] =
      Option(dbi.get(txn, codecK.encode(key).get.bytes.toDirectByteBuffer))
        .map { bytes =>
          codecV.decode(BitVector(bytes)).map(_.value).get
        }

    def put[K, V](txn: Txn[ByteBuffer], key: K, data: V)(
        implicit codecK: Codec[K],
        codecV: Codec[V]
    ): Unit =
      if (!dbi.put(
            txn,
            codecK.encode(key).get.bytes.toDirectByteBuffer,
            codecV.encode(data).map(_.bytes.toDirectByteBuffer).get
          )) {
        throw new Exception(s"could not persist: $data")
      }

    def delete[K](txn: Txn[ByteBuffer], key: K)(implicit codecK: Codec[K]): Unit =
      if (!dbi.delete(txn, codecK.encode(key).get.bytes.toDirectByteBuffer)) {
        throw new Exception(s"could not delete: $key")
      }
  }
}
