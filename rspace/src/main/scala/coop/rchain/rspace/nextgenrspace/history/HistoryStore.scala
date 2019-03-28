package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.nextgenrspace.history.History._

trait HistoryStore {
  def put(tries: List[Trie]): Unit

  def get(key: Blake2b256Hash): Trie
}

object HistoryStore {}
