package coop.rchain.rosette

case class TblObject(entry: Seq[Ob],
                     override val parent: Ob,
                     override val meta: Ob,
                     override val slot: Seq[Ob])
    extends Ob

object TblObject {
  object PLACEHOLDER extends TblObject(null, null, null, null)
}
