package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.store.KeyValueStore
import scodec.bits.ByteVector

import java.nio.ByteBuffer
import scala.language.higherKinds

class RadixStore[F[_]: Sync](store: KeyValueStore[F]) {

  def get(keys: Seq[ByteVector]): F[Seq[Option[Array[Byte]]]] =
    store.get[Array[Byte]](keys.map(copyBVToBuf), copyBufToArr)

  def contains(keys: Seq[ByteVector]): F[Seq[Boolean]] = {
    val results = store.get(keys.map(copyBVToBuf), _ => ())
    results.map(r => r.map(_.nonEmpty))
  }

  def put(kvPairs: Seq[(ByteVector, Array[Byte])]): F[Unit] = {
    val pairs = kvPairs.map { case (k, v) => (copyBVToBuf(k), v) }
    store.put[Array[Byte]](pairs, copyArrToBuf)
  }

  def delete(keys: Seq[ByteVector]): F[Int] =
    store.delete(keys.map(copyBVToBuf))

  private def copyBVToBuf(bv: ByteVector): ByteBuffer = {
    val arr    = bv.toArray
    val newBuf = ByteBuffer.allocateDirect(arr.length)
    newBuf.put(arr).rewind()
  }

  private def copyBufToArr(buf: ByteBuffer): Array[Byte] = {
    val arr = new Array[Byte](buf.rewind.remaining())
    val _   = buf.get(arr)
    arr
  }

  private def copyArrToBuf(arr: Array[Byte]): ByteBuffer = {
    val newBuf = ByteBuffer.allocateDirect(arr.length)
    newBuf.put(arr).rewind()
  }
}
