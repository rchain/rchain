package com.revdefine.node.store.instances.lmdb

import cats.effect.{Resource, Sync}
import cats.syntax.all._
import com.revdefine.node.store.DefineKeyValueStore
import coop.rchain.store.DbEnv
import org.lmdbjava._

import java.nio.ByteBuffer
import scala.collection.JavaConverters._

final case class RevDefineLmdbKeyValueStore[F[_]: Sync](
    getEnvDbi: F[DbEnv[F]]
) extends DefineKeyValueStore[F] {

  private def txnResource(isWrite: Boolean) =
    Resource
      .make[F, (Txn[ByteBuffer], Dbi[ByteBuffer], F[Unit])](for {
        dbEnv                 <- getEnvDbi
        DbEnv(env, dbi, done) = dbEnv
        txn                   = if (isWrite) env.txnWrite else env.txnRead
      } yield (txn, dbi, done)) { case (txn, _, done) => done >> txn.close().pure }
      .map(r => (r._1, r._2))

  // GET
  override def get[T](keys: Seq[ByteBuffer], fromBuffer: ByteBuffer => T): F[Seq[Option[T]]] =
    txnResource(isWrite = false).use {
      case (txn, dbi) =>
        keys.map(x => Option(dbi.get(txn, x)).map(fromBuffer)).pure
    }

  // PUT
  override def put[T](kvPairs: Seq[(ByteBuffer, T)], toBuffer: T => ByteBuffer): F[Unit] = {
    // Buffers for key and value created outside of transaction.
    // Why this helps (or why corruption happens) is not clear but this code will prevent corruption of the database.
    val byteBuffers = kvPairs.map(_.map(toBuffer))
    txnResource(isWrite = true).use {
      case (txn, dbi) =>
        byteBuffers.foreach {
          case (key, value) =>
            if (dbi.put(txn, key, value)) () else ()
        }.pure
    }
  }

  // DELETE
  override def delete(keys: Seq[ByteBuffer]): F[Int] =
    txnResource(isWrite = true).use {
      case (txn, dbi) =>
        keys.foldLeft(0)((acc, key) => if (dbi.delete(txn, key)) acc + 1 else acc).pure
    }

  // ITERATE
  override def iterate: Resource[F, Iterator[(ByteBuffer, ByteBuffer)]] =
    txnResource(isWrite = false)
      .flatMap {
        case (txn, dbi) => Resource.make(dbi.iterate(txn).pure)(_.close().pure)
      }
      .map[F, Iterator[(ByteBuffer, ByteBuffer)]](_.iterator.asScala.map(c => (c.key, c.`val`)))
}
