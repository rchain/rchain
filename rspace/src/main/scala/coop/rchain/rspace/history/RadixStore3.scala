package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.store.KeyValueStore

import java.nio.ByteBuffer
import scala.language.higherKinds

class RadixStore3[F[_]: Sync](store: KeyValueStore[F]) {
  type ArrB = Array[Byte]

  def get(keys: Seq[ArrB]): F[Seq[Option[ByteBuffer]]] =
    store.get(keys.map(copyArrToDirBuf), copyBufToBuf)

  def contains(keys: Seq[ArrB]): F[Seq[Boolean]] = {
    val results = store.get(keys.map(copyArrToDirBuf), _ => ())
    results.map(r => r.map(_.nonEmpty))
  }

  def put(kvPairs: Seq[(ArrB, ByteBuffer)]): F[Unit] = {
    val pairs = kvPairs.map { case (k, v) => (copyArrToDirBuf(k), copyBufToDirBuf(v)) }
    store.put[ByteBuffer](pairs, x => x)
  }

  def delete(keys: Seq[ArrB]): F[Int] =
    store.delete(keys.map(copyArrToDirBuf))

  private def copyBufToDirBuf(buf: ByteBuffer): ByteBuffer = {
    val newBuf = ByteBuffer.allocateDirect(buf.capacity())
    newBuf.put(buf.rewind()).rewind()
  }

  private def copyBufToBuf(buf: ByteBuffer): ByteBuffer = {
    val newBuf = ByteBuffer.allocate(buf.capacity())
    newBuf.put(buf.rewind()).rewind()
  }

  private def copyArrToDirBuf(arr: ArrB): ByteBuffer = {
    val newBuf = ByteBuffer.allocateDirect(arr.length)
    newBuf.put(arr).rewind()
  }
}
