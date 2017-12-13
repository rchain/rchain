package coop.rchain.rosette
import coop.rchain.rosette.Ob.NilMeta

/** StdExtension
  *
  * StdExtension is different from other Rosette objects in the sense that
  * the slot field does not include the meta and parent object.
  */
case class StdExtension(newMeta: Ob,
                        newParent: Ob,
                        override val slot: Seq[Ob] = Seq())
    extends Ob {
  override def meta: Ob = newMeta
  override def parent: Ob = newParent

  def extendWith(keyMeta: Ob, argvec: Tuple): StdExtension =
    if (keyMeta == NilMeta) this else argvec.becomeExtension(keyMeta, this)
}
