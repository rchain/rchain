package coop.rchain.store

import java.nio.ByteBuffer

import cats.Functor
import cats.effect.Sync
import cats.syntax.all._
import scodec.Codec

trait KeyValueStoreSyntax {
  implicit final def sharedSyntaxKeyValueStore[F[_]](store: KeyValueStore[F]): KeyValueStoreOps[F] =
    new KeyValueStoreOps[F](store)
}

final class KeyValueStoreOps[F[_]](
    // KeyValueStore extensions / syntax
    private val store: KeyValueStore[F]
) extends AnyVal {
  // Suffix `1` because overload is not resolved and also because it gets one record

  def get1[T](key: ByteBuffer, fromBuffer: ByteBuffer => T)(implicit f: Functor[F]): F[Option[T]] =
    store.get(Seq(key), fromBuffer).map(_.head)

  def put1[T](key: ByteBuffer, value: T, toBuffer: T => ByteBuffer): F[Unit] =
    store.put(Seq((key, value)), toBuffer)

  def delete1(key: ByteBuffer)(implicit f: Functor[F]): F[Boolean] =
    store.delete(Seq(key)).map(_ == 1)

  def contains(keys: Seq[ByteBuffer])(implicit f: Functor[F]): F[Seq[Boolean]] =
    store.get(keys, _ => ()).map(_.map(_.nonEmpty))

  def contains(key: ByteBuffer)(implicit f: Functor[F]): F[Boolean] =
    contains(Seq(key)).map(_.head)

  // From key-value store, with serializers for K and V, typed store can be created
  def toTypedStore[K, V](kCodec: Codec[K], vCodec: Codec[V])(
      implicit s: Sync[F]
  ): KeyValueTypedStore[F, K, V] = new KeyValueTypedStoreCodec[F, K, V](store, kCodec, vCodec)
}
