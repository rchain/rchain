package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.file.{Path, Paths}

import cats.implicits._
import cats.effect.Sync
import coop.rchain.shared.ByteVectorOps.RichByteVector
import coop.rchain.rspace.Blake2b256Hash
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.{Dbi, Env, EnvFlags, Txn}
import scodec.bits.BitVector

import scala.util.control.NonFatal

trait Store[F[_]] {
  def get(key: Blake2b256Hash): F[Option[BitVector]]
  def put(key: Blake2b256Hash, value: BitVector): F[Unit]
  def get(key: ByteBuffer): F[Option[ByteBuffer]]
  def put(key: ByteBuffer, value: ByteBuffer): F[Unit]
  def put(data: Seq[(Blake2b256Hash, BitVector)]): F[Unit]
  def close(): F[Unit]
}

final case class StoreConfig(
    path: Path,
    mapSize: Long,
    maxDbs: Int = 2,
    maxReaders: Int = 2048,
    flags: List[EnvFlags] = List(EnvFlags.MDB_NOTLS)
)

object StoreInstances {
  def lmdbStore[F[_]: Sync](config: StoreConfig): Store[F] = {
    val env = Env
      .create()
      .setMapSize(config.mapSize)
      .setMaxDbs(config.maxDbs)
      .setMaxReaders(config.maxReaders)
      .open(config.path.toFile, config.flags: _*)
    val dbi = env.openDbi("db", MDB_CREATE)
    LMDBStore(env, dbi)
  }
}

final case class LMDBStore[F[_]: Sync] private[history] (
    env: Env[ByteBuffer],
    dbi: Dbi[ByteBuffer]
) extends Store[F] {

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  // TODO stop throwing exceptions
  private def withTxn[R](txn: Txn[ByteBuffer])(f: Txn[ByteBuffer] => R): R =
    try {
      val ret: R = f(txn)
      txn.commit()
      ret
    } catch {
      case NonFatal(ex) =>
        ex.printStackTrace()
        throw ex
    } finally {
      txn.close()
    }

  private def withReadTxnF[R](f: Txn[ByteBuffer] => R): F[R] =
    Sync[F].delay {
      withTxn(env.txnRead)(f)
    }

  private def withWriteTxnF[R](f: Txn[ByteBuffer] => R): F[R] =
    Sync[F].delay {
      withTxn(env.txnWrite)(f)
    }

  def get(key: Blake2b256Hash): F[Option[BitVector]] = {
    val directKey = key.bytes.toDirectByteBuffer
    get(directKey).map(v => v.map(BitVector(_)))
  }

  override def get(key: ByteBuffer): F[Option[ByteBuffer]] =
    withReadTxnF { txn =>
      dbi.get(txn, key)
    }.map(v => Option(v))

  def put(key: Blake2b256Hash, value: BitVector): F[Unit] = {
    val directKey   = key.bytes.toDirectByteBuffer
    val directValue = value.toByteVector.toDirectByteBuffer
    put(directKey, directValue)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  override def put(key: ByteBuffer, value: ByteBuffer): F[Unit] =
    withWriteTxnF { txn =>
      if (dbi.put(txn, key, value)) {
        ()
      } else {
        throw new RuntimeException("was not able to put data")
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  private[this] def putIfAbsent(txn: Txn[ByteBuffer], key: ByteBuffer, value: ByteBuffer): Unit =
    Option(dbi.get(txn, key)) match {
      case None =>
        if (dbi.put(txn, key, value)) {
          ()
        } else {
          throw new RuntimeException("was not able to put data")
        }
      case _: Some[ByteBuffer] => ()
    }

  def close(): F[Unit] =
    Sync[F].delay {
      env.close()
    }

  override def put(data: Seq[(Blake2b256Hash, BitVector)]): F[Unit] = {
    val byteBuffers = data.map {
      case (key, bytes) =>
        (key.bytes.toDirectByteBuffer, bytes.toByteVector.toDirectByteBuffer)
    }
    withWriteTxnF { txn =>
      byteBuffers.foreach { case (key, value) => putIfAbsent(txn, key, value) }
    }
  }
}
