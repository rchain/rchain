package com.revdefine.node.store

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.store.KeyValueStore

import java.nio.ByteBuffer

trait RevDefineStoreSyntax {
  implicit final def revDefineStoreSyntax[F[_]](
      store: DefineKeyValueStore[F]
  ): RevDefineStoreOps[F] =
    new RevDefineStoreOps[F](store)
}

final class RevDefineStoreOps[F[_]](private val store: DefineKeyValueStore[F]) extends AnyVal {

  def toKeyValueStore(implicit a: Sync[F]): KeyValueStore[F] = new KeyValueStore[F] {
    override def delete(keys: Seq[ByteBuffer]): F[Int] = store.delete(keys)

    override def get[T](keys: Seq[ByteBuffer], fromBuffer: ByteBuffer => T): F[Seq[Option[T]]] =
      store.get(keys, fromBuffer = fromBuffer)

    override def put[T](kvPairs: Seq[(ByteBuffer, T)], toBuffer: T => ByteBuffer): F[Unit] =
      store.put(kvPairs, toBuffer)

    override def iterate[T](f: Iterator[(ByteBuffer, ByteBuffer)] => T): F[T] =
      store.iterate.use[F, T](f(_).pure[F])
  }
}
