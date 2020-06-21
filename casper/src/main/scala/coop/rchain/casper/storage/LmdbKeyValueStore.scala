package coop.rchain.casper.storage

import java.nio.ByteBuffer

import cats.effect.ExitCase.{Canceled, Completed, Error}
import cats.effect.Sync
import cats.effect.syntax.all._
import cats.syntax.all._
import coop.rchain.shared.Resources.withResource
import coop.rchain.store.KeyValueStore
import org.lmdbjava._

import scala.util.control.NonFatal

final case class DbEnv[F[_]](env: Env[ByteBuffer], dbi: Dbi[ByteBuffer], done: F[Unit])

final case class LmdbKeyValueStore[F[_]: Sync](
    getEnvDbi: F[DbEnv[F]]
) extends KeyValueStore[F] {

  private def withReadTxn[T](op: (Txn[ByteBuffer], Dbi[ByteBuffer]) => F[T]): F[T] =
    for {
      // Acquire current LMDB environment and DBI interface
      dbEnv <- getEnvDbi
      // "Done" must be called to mark finished operation with the environment/dbi
      DbEnv(env, dbi, done) = dbEnv
      // Create read transaction
      txnF = Sync[F].delay(env.txnRead)
      // Execute database operation, commit at the end
      result <- txnF.bracketCase(
                 txn =>
                   for {
                     // Execute DB operation within transaction
                     res <- op(txn, dbi)
                     // Commit transaction (read and write)
                     _ <- Sync[F].delay(txn.commit())
                   } yield res
               ) {
                 case (txn, Completed) =>
                   // Close transaction when execution is successful.
                   Sync[F].delay(txn.close()) *> done
                 case (txn, Error(_)) =>
                   // Close transaction on error.
                   Sync[F].delay(txn.close()) *> done
                 case (_, Canceled) =>
                   // When execution is canceled, closing transaction throws NotReadyException
                   //  in bracket `use` function which is not visible in bracket `release` (?).
                   done
               }
    } yield result

  // Ensures transaction is used only on one thread.
  // > A write Transaction may only be used from the thread it was created on.
  // https://lmdb.readthedocs.io/en/release/#threads
  private def withTxnSingleThread[T](
      isWrite: Boolean
  )(op: (Txn[ByteBuffer], Dbi[ByteBuffer]) => T): F[T] =
    for {
      // Acquire current LMDB environment and DBI interface
      dbEnv <- getEnvDbi
      // "Done" must be called to mark finished operation with the environment/dbi
      DbEnv(env, dbi, done) = dbEnv
      // Create read or write transaction
      txn = if (isWrite) env.txnWrite else env.txnRead
      // Execute database operation, commit at the end
      result <- try {
                 // Execute DB operation within transaction
                 val res = op(txn, dbi)
                 // Commit transaction (read and write)
                 txn.commit()
                 // DB result
                 res.pure[F]
               } catch {
                 case NonFatal(ex) =>
                   // Ack done and rethrow error.
                   done *> ex.raiseError[F, T]
               } finally {
                 // Close transaction at the end.
                 txn.close()
               }
      // Ack done
      _ <- done
    } yield result

  def withWriteTxn[T](f: (Txn[ByteBuffer], Dbi[ByteBuffer]) => T): F[T] =
    withTxnSingleThread(isWrite = true)(f)

  // GET
  override def get[T](keys: Seq[ByteBuffer], fromBuffer: ByteBuffer => T): F[Seq[Option[T]]] =
    withReadTxn { (txn, dbi) =>
      Sync[F].delay {
        keys.map(x => Option(dbi.get(txn, x)).map(fromBuffer))
      }
    }

  // PUT
  override def put[T](kvPairs: Seq[(ByteBuffer, T)], toBuffer: T => ByteBuffer): F[Unit] =
    withWriteTxn { (txn, dbi) =>
      kvPairs.foreach {
        case (key, value) =>
          if (dbi.put(txn, key, toBuffer(value))) () else ()
      }
    }

  // DELETE
  override def delete(keys: Seq[ByteBuffer]): F[Int] =
    withWriteTxn { (txn, dbi) =>
      keys.foldLeft(0)((acc, key) => if (dbi.delete(txn, key)) acc + 1 else acc)
    }

  // ITERATE
  override def iterate[T](f: Iterator[(ByteBuffer, ByteBuffer)] => F[T]): F[T] =
    withReadTxn { (txn, dbi) =>
      withResource(dbi.iterate(txn)) { iterator =>
        import scala.collection.JavaConverters._
        f(iterator.asScala.map(c => (c.key, c.`val`)))
      }
    }
}
