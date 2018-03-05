package coop.rchain.rosette

import coop.rchain.rosette.Ob.ABSENT

case class Queue(elems: Tuple) extends Ob {

  def head: Ob = elems.elem.head

  def tail: Ob = elems.elem.last

  def nElems: Ob = Fixnum(elems.elem.length)

  def depth(): Int =
    elems.elem.length

  def isEmpty(): Boolean =
    (nElems.asInstanceOf[Fixnum] == Fixnum(0)).value

  def enqueue(ele: Ob): Queue =
    Queue(Tuple(elems.elem :+ ele))

  def dequeue(): (Ob, Queue) =
    if (isEmpty()) {
      (ABSENT, this)
    } else {
      val v = elems.elem.head
      (v, Queue(Tuple(elems.elem.drop(1))))
    }

  def reset(): Queue =
    Queue.create()

  override def indexedSize: Ob = nElems

  override def nth(i: Int): Option[Ob] = {
    val index = i % elems.elem.length
    elems.elem.lift(index)
  }

  override def setNth(i: Int, v: Ob): Option[Ob] =
    if (isEmpty()) {
      None
    } else {
      val index = i % elems.elem.length
      Some(Queue(Tuple(elems.elem.updated(index, v))))
    }

  override def subObject(i1: Int, i2: Int): Ob = {
    val newElems = elems.makeSlice(i1, i2)
    Queue(Tuple(newElems))
  }

  def patternDequeue(pat: Tuple): (Ob, Queue) = {
    val len = nElems.asInstanceOf[Fixnum].value
    for ((e, i) <- elems.elem.zipWithIndex) {
      e match {
        case msg: Tuple =>
          if (pat.matches(msg)) {
            val newElems = elems.elem.slice(0, i) ++: elems.elem.slice(i + 1, len)
            return (e, Queue(Tuple(newElems)))
          }
        case _ =>
      }
    }
    (ABSENT, this)
  }

  def patternRead(pat: Tuple): Ob = {
    for ((e, i) <- elems.elem.zipWithIndex) {
      e match {
        case msg: Tuple =>
          if (pat.matches(msg)) {
            return e
          }
        case _ =>
      }
    }
    ABSENT
  }

  def dequeueNth(i: Int): (Ob, Queue) = {
    val len = nElems.asInstanceOf[Fixnum].value
    if (i < 0 || i > len) {
      return (ABSENT, this)
    }
    val msg      = elems.elem(i)
    val newElems = elems.elem.slice(0, i) ++: elems.elem.slice(i + 1, len)
    (msg, Queue(Tuple(newElems)))
  }

}

object Queue {

  def create(): Queue = {
    val elems = Tuple.Placeholder
    Queue(elems)
  }

  def apply(elems: Tuple): Queue =
    new Queue(elems)

  def apply(size: Int, meta: Ob, parent: Ob, elems: Tuple): Queue =
    new Queue(elems)

}
