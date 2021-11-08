package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.store.KeyValueStore
import scodec.bits.ByteVector

import java.nio.ByteBuffer
import scala.language.higherKinds

class RadixStore4[F[_]: Sync](store: KeyValueStore[F]) {

  def get(keys: Seq[ByteVector]): F[Seq[Option[ByteVector]]] =
    store.get(keys.map(copyBVToBuf), ByteVector(_))

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
}
