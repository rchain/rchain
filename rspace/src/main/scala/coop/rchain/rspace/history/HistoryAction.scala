package coop.rchain.rspace.history

import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.History.KeyPath

sealed trait HistoryAction {
  def key: KeyPath
}

final case class InsertAction(key: KeyPath, hash: Blake2b256Hash) extends HistoryAction
final case class DeleteAction(key: KeyPath)                       extends HistoryAction
