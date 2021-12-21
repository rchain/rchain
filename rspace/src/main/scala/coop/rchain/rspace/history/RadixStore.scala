package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.store.KeyValueStore
import scodec.bits.ByteVector

import java.nio.ByteBuffer
import scala.language.higherKinds

class RadixStore[F[_]: Sync](store: KeyValueStore[F]) {

  def get(keys: Seq[ByteVector]): F[Seq[Option[ByteVector]]] =
    store.get[ByteVector](keys.map(copyBVToBuf), copyBufToBV)

  def get1(key: ByteVector): F[Option[ByteVector]] =
    get(Seq(key)).map(_.head)

  def contains(keys: Seq[ByteVector]): F[Seq[Boolean]] = {
    val results = store.get(keys.map(copyBVToBuf), _ => ())
    results.map(r => r.map(_.nonEmpty))
  }

  def put(kvPairs: Seq[(ByteVector, ByteVector)]): F[Unit] = {
    val pairs = kvPairs.map { case (k, v) => (copyBVToBuf(k), v) }
    store.put[ByteVector](pairs, copyBVToBuf)
  }

  def delete(keys: Seq[ByteVector]): F[Int] =
    store.delete(keys.map(copyBVToBuf))

  private def copyBVToBuf(bv: ByteVector): ByteBuffer = {
    val arr    = bv.toArray
    val newBuf = ByteBuffer.allocateDirect(arr.length)
    newBuf.put(arr).rewind()
  }

  private def copyBufToBV(buf: ByteBuffer): ByteVector = {
    val arr = new Array[Byte](buf.rewind.remaining())
    val _   = buf.get(arr)
    ByteVector(arr)
  }
}
