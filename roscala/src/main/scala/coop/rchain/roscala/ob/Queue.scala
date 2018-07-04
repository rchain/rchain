package coop.rchain.roscala.ob

import scala.collection.mutable

class Queue(val queue: mutable.Queue[Ob]) extends Ob {
  def enqueue(value: Ob): Unit = queue.enqueue(value)

  def dequeue(): Ob = queue.dequeue()
}

object Queue {
  def apply(): Queue = new Queue(mutable.Queue.empty)

  def apply(tuple: Tuple): Queue = new Queue(mutable.Queue(tuple.value: _*))
}
