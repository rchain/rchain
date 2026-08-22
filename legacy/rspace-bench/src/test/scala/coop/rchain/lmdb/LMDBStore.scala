package coop.rchain.lmdb

import cats.effect.Sync
import org.lmdbjava.{CursorIterable, Dbi, Env, Txn}
import scodec.bits.BitVector

import java.nio.ByteBuffer
import scala.jdk.CollectionConverters._
import scala.util.Using
import scala.util.control.NonFatal

final case class LMDBStore[F[_]: Sync](env: Env[ByteBuffer], dbi: Dbi[ByteBuffer]) {

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

  def withReadTxnF[R](f: Txn[ByteBuffer] => R): F[R] =
    Sync[F].delay {
      withTxn(env.txnRead)(f)
    }

  def withWriteTxnF[R](f: Txn[ByteBuffer] => R): F[R] =
    Sync[F].delay {
      withTxn(env.txnWrite)(f)
    }

  def get(key: ByteBuffer): F[Option[BitVector]] =
    withReadTxnF { txn =>
      Option(dbi.get(txn, key)).map(BitVector(_))
    }

  // TODO: Remove this method. Returned byte buffer is deallocated after transaction is closed which can cause corrupted read.
  def get_WARNING(key: ByteBuffer): F[Option[ByteBuffer]] =
    withReadTxnF { txn =>
      Option(dbi.get(txn, key))
    }

  def get[V](key: Seq[ByteBuffer], fromBuffer: ByteBuffer => V): F[Seq[Option[V]]] =
    withReadTxnF { txn =>
      key.map { k =>
        val v = Option(dbi.get(txn, k))
        v.map(fromBuffer)
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def put(key: ByteBuffer, value: ByteBuffer): F[Unit] =
    withWriteTxnF { txn =>
      if (!dbi.put(txn, key, value)) {
        throw new RuntimeException("was not able to put data")
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def put(kvPairs: List[(ByteBuffer, ByteBuffer)]): F[Unit] =
    withWriteTxnF { txn =>
      kvPairs.foreach {
        case (key, value) =>
          if (!dbi.put(txn, key, value)) {
            throw new RuntimeException("was not able to put data")
          }
      }
    }

  def delete(keys: List[ByteBuffer]): F[Int] =
    withWriteTxnF { txn =>
      keys.foldLeft(0) { (deletedCount, key) =>
        if (dbi.delete(txn, key)) deletedCount + 1 else deletedCount
      }
    }

  def iterate[R](f: Iterator[CursorIterable.KeyVal[ByteBuffer]] => R): F[R] =
    withReadTxnF { txn =>
      Using.resource(dbi.iterate(txn)) { iterator =>
        f(iterator.iterator.asScala)
      }
    }

  def drop: F[Unit] =
    withWriteTxnF { txn =>
      dbi.drop(txn)
    }

  def close(): F[Unit] =
    Sync[F].delay {
      env.close()
    }
}
