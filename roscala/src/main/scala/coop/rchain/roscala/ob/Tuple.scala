package coop.rchain.roscala.ob

class Tuple(val value: Array[Ob]) extends Ob {
  def apply(n: Int): Ob = value(n)

  def update(arg: Int, ob: Ob): Unit = value.update(arg, ob)

  def numberOfElements(): Int = value.length
}

object Tuple {
  def apply(value: Array[Ob]): Tuple = new Tuple(value)
}

case object Nil extends Ob
