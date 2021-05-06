package coop.rchain.rspace.history

import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.serializers.ScodecSerialize._
import coop.rchain.shared.syntax._
import coop.rchain.store.KeyValueStore
import scodec.bits.BitVector

trait HistoryStore[F[_]] {
  def put(tries: List[Trie]): F[Unit]

  def get(key: Blake2b256Hash): F[Trie]
}

object HistoryStoreInstances {
  type KVData = (Blake2b256Hash, BitVector)

  def historyStore[F[_]: Sync](store: KeyValueStore[F]): HistoryStore[F] = new HistoryStore[F] {
    val typedStore = store.toTypedStore(Blake2b256Hash.codecPureBlake2b256Hash, codecTrie)

    override def get(key: Blake2b256Hash): F[Trie] = typedStore.getOrElse(key, EmptyTrie)

    override def put(tries: List[Trie]): F[Unit] =
      typedStore.putIfAbsent(tries.map(t => (Trie.hash(t), t)))
  }
}
