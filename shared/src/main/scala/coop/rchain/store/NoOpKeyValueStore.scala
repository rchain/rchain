package coop.rchain.store

import cats.Applicative
import cats.syntax.all._

import java.nio.ByteBuffer

/**
  * No operation implementation of [[KeyValueStore]]
  *
  * Useful in places where we want to have disabled or dummy storage.
  */
final case class NoOpKeyValueStore[F[_]: Applicative]() extends KeyValueStore[F] {
  override def get[T](keys: Seq[ByteBuffer], fromBuffer: ByteBuffer => T): F[Seq[Option[T]]] =
    Seq[Option[T]]().pure[F]

  override def put[T](kvPairs: Seq[(ByteBuffer, T)], toBuffer: T => ByteBuffer): F[Unit] =
    ().pure[F]

  override def delete(keys: Seq[ByteBuffer]): F[Int] = 0.pure[F]

  override def iterate[T](f: Iterator[(ByteBuffer, ByteBuffer)] => T): F[T] =
    f(Seq().iterator).pure[F]
}
