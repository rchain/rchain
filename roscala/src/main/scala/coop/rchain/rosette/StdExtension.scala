package coop.rchain.rosette
import coop.rchain.rosette.Ob.NilMeta

case class StdExtension(override val meta: Ob,
                        override val parent: Ob,
                        override val slot: Slot = Slot.Placeholder)
    extends Ob {
  def extendWith(keyMeta: Ob, argvec: Tuple): Option[StdExtension] =
    if (keyMeta == NilMeta) Some(this)
    else argvec.becomeExtension(keyMeta, this)
}
