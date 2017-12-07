package coop.rchain.rosette

case class TblObject(entry: Seq[Ob], override val slot: Slot) extends Ob

object TblObject {
  object PLACEHOLDER extends TblObject(Nil, Slot.Placeholder)
}
