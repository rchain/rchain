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

  /* Assumes that last element in `Tuple` is a `Tuple` */
  def unwind(): Tuple = {
    val prefix = this.value.take(this.value.length - 1)
    val rest   = this.value.last.asInstanceOf[Tuple]
    new Tuple(prefix ++ rest.value)
  }

  def numberOfElements(): Int = value.length
}

object Tuple {
  def apply(value: Array[Ob]): Tuple = new Tuple(value)

  def apply(obs: Ob*): Tuple = new Tuple(obs.toArray[Ob])

  def apply(a: Int, b: Ob): Tuple   = Tuple(Array.fill(a)(b))
  def cons(ob: Ob, t: Tuple): Tuple = Tuple(ob +: t.value)
}

object Nil extends Tuple(Array.empty[Ob])
