package coop.rchain.rspace.history

import cats.Functor
import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.AttemptOpsF.RichAttempt
import coop.rchain.store.{KeyValueStore, KeyValueTypedStore}
import coop.rchain.shared.syntax._
import scodec.DecodeResult
import scodec.bits.BitVector

trait HistoryStore[F[_]] {
  def put(tries: List[Trie]): F[Unit]

  def get(key: Blake2b256Hash): F[Trie]

  def close(): F[Unit]
}

object HistoryStoreInstances {
  type KVData = (Blake2b256Hash, BitVector)
  def historyStore[F[_]: Sync](store: Store[F]): HistoryStore[F] = new HistoryStore[F] {
    // TODO put list
    override def put(tries: List[Trie]): F[Unit] = {

      def asEncoded(t: Trie): F[KVData] =
        for {
          // TODO: the key hash is not the hash of stored binary value but it's
          //  the hash of each case of Trie (see `Trie.hash`)
          //  - this makes it much harder to validate binary value because it must be encoded first
          b <- Trie.codecTrie.encode(t).get
          k = Trie.hash(t)
        } yield (k, b)

      for {
        asKeyValue <- tries traverse asEncoded
        storeRes   <- store.put(asKeyValue)
      } yield (storeRes)
    }

    override def get(key: Blake2b256Hash): F[Trie] =
      for {
        maybeBytes <- store.get(key)
        result     <- maybeBytes.traverse(decodeTrieUnsafe(key))
      } yield (result.map(_.value).getOrElse(EmptyTrie))

    private def decodeTrieUnsafe(key: Blake2b256Hash)(bytes: BitVector) =
      Trie.codecTrie.decode(bytes).get.handleErrorWith { ex: Throwable =>
        new Exception(s"Critical error: decoding value for key ${key.bytes.toHex} failed.", ex)
          .raiseError[F, DecodeResult[Trie]]
      }

    override def close(): F[Unit] = store.close()
  }

  def historyStore[F[_]: Sync](store: KeyValueStore[F]): HistoryStore[F] = new HistoryStore[F] {
    val typedStore = store.toTypedStore(Blake2b256Hash.codecPureBlake2b256Hash, Trie.codecTrie)

    override def close(): F[Unit] = ().pure[F]

    override def get(key: Blake2b256Hash): F[Trie] = typedStore.getOrElse(key, EmptyTrie)

    override def put(tries: List[Trie]): F[Unit] = typedStore.put(tries.map(t => (Trie.hash(t), t)))
  }
}
