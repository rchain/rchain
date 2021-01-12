package coop.rchain.store

import java.nio.ByteBuffer

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.shared.Resources.withResource
import org.lmdbjava._

import scala.util.control.NonFatal

final case class DbEnv[F[_]](env: Env[ByteBuffer], dbi: Dbi[ByteBuffer], done: F[Unit])

final case class LmdbKeyValueStore[F[_]: Sync](
    getEnvDbi: F[DbEnv[F]]
) extends KeyValueStore[F] {

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
      // Execute database operation, commit at the end
      result <- {
        // Create read or write transaction
        val txn = if (isWrite) env.txnWrite else env.txnRead
        try {
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
      }
      // Ack done
      _ <- done
    } yield result

  def withReadTxn[T](f: (Txn[ByteBuffer], Dbi[ByteBuffer]) => T): F[T] =
    withTxnSingleThread(isWrite = false)(f)

  def withWriteTxn[T](f: (Txn[ByteBuffer], Dbi[ByteBuffer]) => T): F[T] =
    withTxnSingleThread(isWrite = true)(f)

  // lmdb can only work with direct buffer
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  private def forceDirectBuffer(b: ByteBuffer): ByteBuffer =
    if (b.isDirect) b
    else {
      val buffer: ByteBuffer = ByteBuffer.allocateDirect(b.remaining())
      buffer.put(b)
      buffer.flip()
      buffer
    }

  // GET
  override def get[T](keys: Seq[ByteBuffer], fromBuffer: ByteBuffer => T): F[Seq[Option[T]]] =
    withTxnSingleThread(isWrite = false) { (txn, dbi) =>
      keys.map(x => Option(dbi.get(txn, forceDirectBuffer(x))).map(fromBuffer))
    }

  // PUT
  override def put[T](kvPairs: Seq[(ByteBuffer, T)], toBuffer: T => ByteBuffer): F[Unit] = {
    // Buffers for key and value created outside of transaction.
    // Why this helps (or why corruption happens) is not clear but this code will prevent corruption of the database.
    val byteBuffers = kvPairs.map {
      case (key, value) => (forceDirectBuffer(key), forceDirectBuffer(toBuffer(value)))
    }
    withWriteTxn { (txn, dbi) =>
      byteBuffers.foreach {
        case (key, value) =>
          if (dbi.put(txn, key, value)) () else ()
      }
    }
  }

  // DELETE
  override def delete(keys: Seq[ByteBuffer]): F[Int] =
    withWriteTxn { (txn, dbi) =>
      keys.foldLeft(0)((acc, key) => if (dbi.delete(txn, forceDirectBuffer(key))) acc + 1 else acc)
    }

  // ITERATE
  override def iterate[T](f: Iterator[(ByteBuffer, ByteBuffer)] => T): F[T] =
    withReadTxn { (txn, dbi) =>
      withResource(dbi.iterate(txn)) { iterator =>
        import scala.collection.JavaConverters._
        f(iterator.iterator.asScala.map(c => (c.key, c.`val`)))
      }
    }
}
