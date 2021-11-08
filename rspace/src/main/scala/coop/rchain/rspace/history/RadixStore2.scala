package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.shared.ByteVectorOps.RichByteVector
import coop.rchain.store.KeyValueStore
import scodec.bits.ByteVector

import java.nio.ByteBuffer
import scala.language.higherKinds

class RadixStore2[F[_]: Sync](store: KeyValueStore[F]) {
  def get(keys: Seq[ByteVector]): F[Seq[Option[ByteBuffer]]] =
    store.get(keys.map(_.toDirectByteBuffer), copyToBuffer)

  def contains(keys: Seq[ByteVector]): F[Seq[Boolean]] = {
    val results = store.get(keys.map(_.toDirectByteBuffer), _ => ())
    results.map(r => r.map(_.nonEmpty))
  }

  def put(kvPairs: Seq[(ByteVector, ByteBuffer)]): F[Unit] = {
    val pairs = kvPairs.map { case (k, v) => (k.toDirectByteBuffer, copyToDirectBuffer(v)) }
    store.put[ByteBuffer](pairs, x => x)
  }

  def delete(keys: Seq[ByteVector]): F[Int] =
    store.delete(keys.map(_.toDirectByteBuffer))

//  /*
  def copyToDirectBuffer(buf: ByteBuffer): ByteBuffer = {
    val newBuf = ByteBuffer.allocateDirect(buf.capacity())
    newBuf.put(buf).rewind()
  }

  def copyToBuffer(buf: ByteBuffer): ByteBuffer = {
    val newBuf = ByteBuffer.allocate(buf.capacity())
    newBuf.put(buf).rewind()
  }
//   */

  def blackMagic(buf: ByteBuffer): ByteBuffer =
    buf.duplicate()
}
