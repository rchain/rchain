package coop.rchain.roscala.ob

class Tuple(val value: Array[Ob]) extends Ob {
  def apply(i: Int): Ob = value(i)

  def update(arg: Int, ob: Ob): Unit = value.update(arg, ob)

  def numberOfElements(): Int = value.size
}

case object Nil extends Ob
