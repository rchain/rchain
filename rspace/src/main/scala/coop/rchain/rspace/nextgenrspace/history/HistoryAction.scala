package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.Blake2b256Hash

sealed trait HistoryAction {
  def key: List[Byte]
}

final case class InsertAction(key: List[Byte], hash: Blake2b256Hash) extends HistoryAction
final case class DeleteAction(key: List[Byte])                       extends HistoryAction
