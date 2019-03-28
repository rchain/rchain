package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.Blake2b256Hash

sealed trait HistoryAction[K] {
  def key: K
}

final case class InsertAction[K](key: K, hash: Blake2b256Hash) extends HistoryAction[K]
final case class DeleteAction[K](key: K)                       extends HistoryAction[K]
