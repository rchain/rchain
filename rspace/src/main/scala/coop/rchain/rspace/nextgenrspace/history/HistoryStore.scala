package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.nextgenrspace.history.History._

trait HistoryStore[F[_]] {
  def put(tries: List[Trie]): F[Unit]

  def get(key: Blake2b256Hash): F[Trie]
}

object HistoryStore {}
