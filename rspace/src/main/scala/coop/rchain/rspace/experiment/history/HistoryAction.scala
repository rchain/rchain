package coop.rchain.rspace
package experiment
package history

import coop.rchain.rspace.history.History.KeyPath

sealed trait HistoryAction {
  def key: KeyPath
}

final case class InsertAction(key: KeyPath, hash: Blake2b256Hash) extends HistoryAction
final case class DeleteAction(key: KeyPath)                       extends HistoryAction
