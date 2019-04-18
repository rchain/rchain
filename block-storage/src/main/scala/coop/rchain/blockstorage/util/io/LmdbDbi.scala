package coop.rchain.blockstorage.util.io

import java.io.{IOException, PrintWriter, StringWriter}

import cats.implicits._
import cats.effect.{ExitCase, Sync}
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.shared.Log
import coop.rchain.shared.Resources.withResource
import org.lmdbjava._

import scala.collection.JavaConverters._
import scala.util.control.NonFatal

final case class LmdbDbi[F[_]: Sync: Log: RaiseIOError, T](
    env: Env[T],
    dbi: Dbi[T]
) {
  private[this] def withTxn[R](txnThunk: => Txn[T])(f: Txn[T] => R): F[R] =
    Sync[F].bracketCase(Sync[F].delay(txnThunk)) { txn =>
      Sync[F].delay {
        val r = f(txn)
        txn.commit()
        r
      }
    } {
      case (txn, ExitCase.Error(NonFatal(ex))) =>
        Log[F].error("Could not process LMDB transaction", ex) *>
          Sync[F].delay(txn.close()) *>
          Sync[F].raiseError(ex)
      case (txn, _) => Sync[F].delay(txn.close())
    }

  def withWriteTxn(f: Txn[T] => Unit): F[Unit] =
    withTxn(env.txnWrite())(f)

  def withReadTxn[R](f: Txn[T] => R): F[R] =
    withTxn(env.txnRead())(f)

  def get(txn: Txn[T], key: T): Option[T] =
    Option(dbi.get(txn, key))

  def iterate[R](txn: Txn[T])(f: Iterator[CursorIterator.KeyVal[T]] => R): R =
    withResource(dbi.iterate(txn)) { iterator =>
      f(iterator.asScala)
    }

  def put(txn: Txn[T], key: T, value: T, flags: PutFlags*): Boolean =
    dbi.put(txn, key, value, flags: _*)

  def drop(txn: Txn[T]): Unit =
    dbi.drop(txn)

  def close: F[Unit] =
    Sync[F].defer {
      try {
        env.close().pure[F]
      } catch {
        case e: IOException =>
          RaiseIOError[F].raise[Unit](ClosingFailed(e))
        case NonFatal(e) =>
          RaiseIOError[F].raise[Unit](UnexpectedIOError(e))
      }
    }
}
