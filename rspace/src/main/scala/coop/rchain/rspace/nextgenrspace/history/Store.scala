package coop.rchain.rspace.nextgenrspace.history

import java.nio.ByteBuffer
import java.nio.file.Paths

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
  def close(): F[Unit]
}

final case class StoreConfig(
    path: String,
    mapSize: Long,
    maxDbs: Int,
    maxReaders: Int,
    flags: List[EnvFlags]
)

object StoreInstances {
  def lmdbStore[F[_]: Sync](config: StoreConfig): Store[F] = {
    val env = Env
      .create()
      .setMapSize(config.mapSize)
      .setMaxDbs(config.maxDbs)
      .setMaxReaders(config.maxReaders)
      .open(Paths.get(config.path).toFile, config.flags: _*)
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
    withReadTxnF { txn =>
      dbi.get(txn, directKey)
    }.map(v => Option(v).map(BitVector(_)))
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def put(key: Blake2b256Hash, value: BitVector): F[Unit] = {
    val directKey   = key.bytes.toDirectByteBuffer
    val directValue = value.toByteVector.toDirectByteBuffer
    withWriteTxnF { txn =>
      if (dbi.put(txn, directKey, directValue)) {
        ()
      } else {
        throw new RuntimeException("was not able to put data")
      }
    }
  }

  def close(): F[Unit] =
    Sync[F].delay {
      env.close()
    }
}
