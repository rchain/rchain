package coop.rchain.rspace.history

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.shared.ByteVectorOps.RichByteVector
import coop.rchain.store.KeyValueStore
import scodec.bits.ByteVector

import scala.collection.concurrent.TrieMap
import scala.language.higherKinds

class RadixStore[F[_]: Sync](store: KeyValueStore[F], useCaching: Boolean = false) {
  private[rspace] val cache: TrieMap[ByteVector, ByteVector] = TrieMap.empty

  def get(keys: Seq[ByteVector]): F[Seq[Option[ByteVector]]] =
    if (useCaching) {
      ???
    } else
      store.get(keys.map(_.toDirectByteBuffer), ByteVector(_))

  def contains(keys: Seq[ByteVector]): F[Seq[Boolean]] =
    if (useCaching) {
      ???
    } else {
      val results = store.get(keys.map(_.toDirectByteBuffer), _ => ())
      results.map(r => Seq(r.nonEmpty))
    }

  def put(kvPairs: Seq[(ByteVector, ByteVector)]): F[Unit] =
    if (useCaching) {
      ???
    } else {
      val pairs = kvPairs.map { case (k, v) => (k.toDirectByteBuffer, v) }
      store.put[ByteVector](pairs, _.toDirectByteBuffer)
    }

  def delete(keys: Seq[ByteVector]): F[Int] =
    if (useCaching) {
      ???
    } else {
      store.delete(keys.map(_.toDirectByteBuffer))
    }

  def saveCacheInStore: F[Unit] = ???

  def clearCache: F[Unit] = Sync[F].delay {
    cache.clear()
  }
}
