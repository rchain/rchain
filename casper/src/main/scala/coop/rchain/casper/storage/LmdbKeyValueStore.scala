package coop.rchain.casper.storage

import java.nio.ByteBuffer

import cats.effect.ExitCase.{Canceled, Completed, Error}
import cats.effect.Sync
import cats.effect.syntax.all._
import cats.syntax.all._
import coop.rchain.shared.Resources.withResource
import coop.rchain.store.KeyValueStore
import org.lmdbjava._

final case class DbEnv[F[_]](env: Env[ByteBuffer], dbi: Dbi[ByteBuffer], done: F[Unit])

final case class LmdbKeyValueStore[F[_]: Sync](
    getEnvDbi: F[DbEnv[F]]
) extends KeyValueStore[F] {

  private def withTxn[T](
      isWrite: Boolean
  )(op: (Txn[ByteBuffer], Dbi[ByteBuffer]) => F[T]): F[T] =
    for {
      // Acquire current LMDB environment and DBI interface
      dbEnv <- getEnvDbi
      // "Done" must be called to mark finished operation with the environment/dbi
      DbEnv(env, dbi, done) = dbEnv
      // Create read or write transaction
      txnF = Sync[F].delay(if (isWrite) env.txnWrite else env.txnRead)
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

  def withReadTxnF[T](f: (Txn[ByteBuffer], Dbi[ByteBuffer]) => F[T]): F[T] =
    withTxn(isWrite = false)(f)

  def withWriteTxnF[T](f: (Txn[ByteBuffer], Dbi[ByteBuffer]) => F[T]): F[T] =
    withTxn(isWrite = true)(f)

  // GET
  override def get[T](keys: Seq[ByteBuffer], fromBuffer: ByteBuffer => T): F[Seq[Option[T]]] =
    withReadTxnF { (txn, dbi) =>
      import cats.instances.vector._
      keys.toVector
        .traverse(x => Sync[F].delay(Option(dbi.get(txn, x)).map(fromBuffer)))
        .map(_.toSeq)
    }

  // PUT
  override def put[T](kvPairs: Seq[(ByteBuffer, T)], toBuffer: T => ByteBuffer): F[Unit] =
    withWriteTxnF { (txn, dbi) =>
      Sync[F].delay(kvPairs.foreach {
        case (key, value) =>
          if (dbi.put(txn, key, toBuffer(value))) () else ()
      })
    }

  // DELETE
  override def delete(keys: Seq[ByteBuffer]): F[Int] =
    withWriteTxnF { (txn, dbi) =>
      Sync[F].delay(keys.foldLeft(0)((acc, key) => if (dbi.delete(txn, key)) acc + 1 else acc))
    }

  // ITERATE
  override def iterate[T](f: Iterator[(ByteBuffer, ByteBuffer)] => F[T]): F[T] =
    withReadTxnF { (txn, dbi) =>
      withResource(dbi.iterate(txn)) { iterator =>
        import scala.collection.JavaConverters._
        f(iterator.asScala.map(c => (c.key, c.`val`)))
      }
    }
}
