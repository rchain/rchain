package coop.rchain.store

import cats.effect.Sync
import scodec.bits.ByteVector

import java.nio.ByteBuffer
import scala.collection.concurrent.TrieMap

final case class InMemoryKeyValueStore[F[_]: Sync]() extends KeyValueStore[F] {

  val state = TrieMap[ByteBuffer, ByteVector]()

  override def get[T](keys: Seq[ByteBuffer], fromBuffer: ByteBuffer => T): F[Seq[Option[T]]] =
    Sync[F].delay(
      keys.map(state.get).map(_.map(_.toByteBuffer).map(fromBuffer))
    )

  override def put[T](kvPairs: Seq[(ByteBuffer, T)], toBuffer: T => ByteBuffer): F[Unit] =
    Sync[F].delay(
      kvPairs
        .foreach {
          case (k, v) =>
            state.put(k, ByteVector(toBuffer(v)))
        }
    )

  override def delete(keys: Seq[ByteBuffer]): F[Int] =
    Sync[F].delay(keys.map(state.remove).count(_.nonEmpty))

  override def iterate[T](f: Iterator[(ByteBuffer, ByteBuffer)] => T): F[T] =
    Sync[F].delay {
      val iter = state.toIterator.map { case (k, v) => (k, v.toByteBuffer) }
      f(iter)
    }

  def clear(): Unit = state.clear()

  def numRecords(): Int = state.size

  def sizeBytes(): Long =
    state.map { case (byteBuffer, byteVector) => byteBuffer.capacity + byteVector.size }.sum

}
