package coop.rchain.roscala.ob

import scala.collection.mutable

class Queue(elems: mutable.Queue[Ob]) extends Ob {
  def enqueue(value: Ob): Unit = elems.enqueue(value)

  def dequeue(): Ob = elems.dequeue()
}

object Queue {
  def apply(): Queue = new Queue(mutable.Queue.empty)

  def apply(tuple: Tuple): Queue = new Queue(mutable.Queue(tuple.value: _*))
}
