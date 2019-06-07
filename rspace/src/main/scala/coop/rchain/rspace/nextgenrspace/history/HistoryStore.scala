package coop.rchain.rspace.nextgenrspace.history

import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.AttemptOps.RichAttempt

trait HistoryStore[F[_]] {
  def put(tries: List[Trie]): F[Unit]

  def get(key: Blake2b256Hash): F[Trie]

  def close(): F[Unit]
}

object HistoryStoreInstances {
  def historyStore[F[_]: Sync](store: Store[F]): HistoryStore[F] = new HistoryStore[F] {
    // TODO put list
    override def put(tries: List[Trie]): F[Unit] = {
      val data = tries
        .map { t =>
          val key   = Trie.hash(t)
          val bytes = Trie.codecTrie.encode(t).get
          (key, bytes)
        }
      store.put(data)
    }

    override def get(key: Blake2b256Hash): F[Trie] =
      for {
        maybeBytes <- store.get(key)
        result = maybeBytes
          .map(
            bytes => Trie.codecTrie.decode(bytes).get.value
          )
          .getOrElse(EmptyTrie)
      } yield result

    override def close(): F[Unit] = store.close()
  }
}
