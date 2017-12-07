package coop.rchain.rosette

case class TblObject(entry: Seq[Ob], override val slot: Seq[Ob] = Seq())
    extends Ob

object TblObject {
  object PLACEHOLDER extends TblObject(Nil)
}
