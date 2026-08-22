package coop.rchain.store

import java.nio.ByteBuffer

trait KeyValueStore[F[_]] {
  def get[T](keys: Seq[ByteBuffer], fromBuffer: ByteBuffer => T): F[Seq[Option[T]]]

  def put[T](kvPairs: Seq[(ByteBuffer, T)], toBuffer: T => ByteBuffer): F[Unit]

  def delete(keys: Seq[ByteBuffer]): F[Int]

  def iterate[T](f: Iterator[(ByteBuffer, ByteBuffer)] => T): F[T]
}
