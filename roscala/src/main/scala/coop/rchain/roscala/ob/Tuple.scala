package coop.rchain.roscala.ob

class Tuple(val value: Array[Ob]) extends Ob {
  def apply(n: Int): Ob = value(n)

  def becomeExtension(newMeta: Meta, newParent: Ob): Extension = {
    val extension = new Extension()
    extension.meta = newMeta
    extension.parent = newParent
    extension.slot ++= value

    extension
  }

  def update(arg: Int, ob: Ob): Unit = value.update(arg, ob)

  def numberOfElements(): Int = value.length
}

object Tuple {
  def apply(value: Array[Ob]): Tuple = new Tuple(value)

  def apply(obs: Ob*): Tuple = new Tuple(obs.toArray[Ob])
}

object Nil extends Ob
