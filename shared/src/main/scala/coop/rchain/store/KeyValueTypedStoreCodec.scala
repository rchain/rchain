package coop.rchain.store

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.shared.ByteVectorOps.RichByteVector
import scodec.Codec
import scodec.bits.BitVector

class KeyValueTypedStoreCodec[F[_]: Sync, K, V](
    store: KeyValueStore[F],
    kCodec: Codec[K],
    vCodec: Codec[V]
) extends KeyValueTypedStore[F, K, V] {
  // TODO: create specialized exceptions for Codec errors
  def encodeKey(key: K): F[BitVector] =
    kCodec
      .encode(key)
      .fold(
        err => new Exception(err.message).raiseError[F, BitVector],
        _.pure[F]
      )

  def decodeKey(bytes: BitVector): F[K] =
    kCodec
      .decodeValue(bytes)
      .fold(
        err => new Exception(err.message).raiseError[F, K],
        _.pure[F]
      )

  def encodeValue(value: V): F[BitVector] =
    vCodec
      .encode(value)
      .fold(
        err => new Exception(err.message).raiseError[F, BitVector],
        _.pure[F]
      )

  def decodeValue(bytes: BitVector): F[V] =
    vCodec
      .decodeValue(bytes)
      .fold(
        err => new Exception(err.message).raiseError[F, V],
        _.pure[F]
      )

  import cats.instances.option._
  import cats.instances.vector._

  override def get(keys: Seq[K]): F[Seq[Option[V]]] =
    for {
      keysBitVector <- keys.toVector.traverse(encodeKey)
      keysBuf       = keysBitVector.map(_.toByteVector.toDirectByteBuffer)
      valuesBytes   <- store.get(keysBuf, BitVector(_))
      values        <- valuesBytes.toVector.traverse(_.traverse(decodeValue))
    } yield values

  override def exist(keys: Seq[K]): F[Seq[Boolean]] =
    for {
      keysBitVector <- keys.toVector.traverse(encodeKey)
      keysBuf       = keysBitVector.map(_.toByteVector.toDirectByteBuffer)
      valuesBytes   <- store.get(keysBuf, identity)
    } yield valuesBytes.map(_.nonEmpty)

  override def put(kvPairs: Seq[(K, V)]): F[Unit] =
    for {
      pairsBitVector <- kvPairs.toVector.traverse {
                         case (k, v) => encodeKey(k).map2(encodeValue(v))((x, y) => (x, y))
                       }
      pairs = pairsBitVector.map { case (k, v) => (k.toByteVector.toDirectByteBuffer, v) }
      _     <- store.put[BitVector](pairs, _.toByteVector.toDirectByteBuffer)
    } yield ()

  override def delete(keys: Seq[K]): F[Int] =
    for {
      keysBitVector <- keys.toVector.traverse(encodeKey)
      keysBuf       = keysBitVector.map(_.toByteVector.toDirectByteBuffer)
      deletedCount  <- store.delete(keysBuf)
    } yield deletedCount

  override def contains(keys: Seq[K]): F[Seq[Boolean]] =
    for {
      keysBitVector <- keys.toVector.traverse(encodeKey)
      keysBuf       = keysBitVector.map(_.toByteVector.toDirectByteBuffer)
      results       <- store.get(keysBuf, _ => ())
    } yield results.map(_.nonEmpty)

  override def toMap: F[Map[K, V]] =
    for {
      valuesBytes <- store.iterate(
                      _.map { case (k, v) => (BitVector(k), BitVector(v)) }.toVector
                    )
      values <- valuesBytes.traverse {
                 case (k, v) =>
                   for {
                     key   <- decodeKey(k)
                     value <- decodeValue(v)
                   } yield (key, value)

               }
    } yield values.toMap
}
