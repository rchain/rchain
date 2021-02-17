package coop.rchain.store

trait KeyValueTypedStore[F[_], K, V] {
  def get(keys: Seq[K]): F[Seq[Option[V]]]

  def put(kvPairs: Seq[(K, V)]): F[Unit]

  def delete(keys: Seq[K]): F[Int]

  def contains(keys: Seq[K]): F[Seq[Boolean]]

  /**
    * Efficient way to iterate and filter the whole KV store
    *
    * @param pf Partial function to project and filter values
    */
  def collect[T](pf: PartialFunction[(K, () => V), T]): F[Seq[T]]

  def toMap: F[Map[K, V]]
}
