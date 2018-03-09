package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._
import Catscontrib._

trait Kvs[F[_], K, V] {
  def keys: F[Vector[K]]
  def get(k: K): F[Option[V]]
  def put(k: K, v: V): F[Unit]
  def delete(k: K): F[Unit]
}

object Kvs extends KvsInstances {
  def apply[F[_], K, V](implicit K: Kvs[F, K, V]): Kvs[F, K, V] = K

  def forTrans[F[_]: Monad, K, V, T[_[_], _]: MonadTrans](
      implicit K: Kvs[F, K, V]): Kvs[T[F, ?], K, V] =
    new Kvs[T[F, ?], K, V] {
      def keys            = K.keys.liftM[T]
      def get(k: K)       = K.get(k).liftM[T]
      def put(k: K, v: V) = K.put(k, v).liftM[T]
      def delete(k: K)    = K.delete(k).liftM[T]
    }

  class InMemoryKvs[F[_]: Capture, K, V] extends Kvs[F, K, V] {
    val m = scala.collection.concurrent.TrieMap[K, V]()

    def keys: F[Vector[K]] = Capture[F].capture {
      m.keys.toVector
    }
    def get(k: K): F[Option[V]] = Capture[F].capture {
      m.get(k)
    }
    def put(k: K, v: V): F[Unit] = Capture[F].capture {
      m += (k -> v)
    }

    def delete(k: K): F[Unit] = Capture[F].capture {
      m -= k
    }
  }

}

sealed abstract class KvsInstances {

  implicit def eitherTKvs[K, V, E, F[_]: Monad: Kvs[?[_], K, V]]: Kvs[EitherT[F, E, ?], K, V] =
    Kvs.forTrans[F, K, V, EitherT[?[_], E, ?]]

  implicit def writerTKvs[K, V, W: Monoid, F[_]: Monad: Kvs[?[_], K, V]]
    : Kvs[WriterT[F, W, ?], K, V] =
    Kvs.forTrans[F, K, V, WriterT[?[_], W, ?]]
}
