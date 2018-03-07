package coop.rchain.rosette

import coop.rchain.rosette.Ob.{ABSENT, NIV}

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

  def dequeue(): (Queue, Ob) =
    if (isEmpty()) {
      (this, ABSENT)
    } else {
      val v = elems.elem.head
      (Queue(Tuple(elems.elem.drop(1))), v)
    }

  def reset(): Queue =
    Queue.create()

  override def indexedSize: Ob = nElems

  override def nth(i: Int): Option[Ob] = {
    val index = i % elems.elem.length
    elems.elem.lift(index)
  }

  override def setNth(i: Int, v: Ob): Option[Queue] =
    if (isEmpty()) {
      None
    } else {
      val index = i % elems.elem.length
      Some(Queue(Tuple(elems.elem.updated(index, v))))
    }

  override def subObject(i1: Int, i2: Int): Queue = {
    val newElems = elems.makeSlice(i1, i2)
    Queue(Tuple(newElems))
  }

  def patternDequeue(pat: Tuple): (Queue, Ob) = {
    val len = nElems.asInstanceOf[Fixnum].value
    val maybeMatchedPair = elems.elem.zipWithIndex.find {
      case (msg: Tuple, index: Int) => pat.matches(msg)
      case _                        => false
    }
    maybeMatchedPair
      .map { p =>
        val ele      = p._1
        val index    = p._2
        val newElems = elems.elem.slice(0, index) ++: elems.elem.slice(index + 1, len)
        (Queue(Tuple(newElems)), ele)
      }
      .getOrElse(this, ABSENT)
  }

  def patternRead(pat: Tuple): Ob = {
    val maybeMatchedEle = elems.elem.find {
      case msg: Tuple => pat.matches(msg)
      case _          => false
    }
    maybeMatchedEle.getOrElse(ABSENT)
  }

  def dequeueNth(i: Int): (Queue, Ob) = {
    val len = nElems.asInstanceOf[Fixnum].value
    if (i < 0 || i > len) {
      (this, ABSENT)
    } else {
      val msg      = elems.elem(i)
      val newElems = elems.elem.slice(0, i) ++: elems.elem.slice(i + 1, len)
      (Queue(Tuple(newElems)), msg)
    }
  }

}

object Queue {

  val DefaultQueueSize = 4

  def create(): Queue = {
    val elems = Tuple(DefaultQueueSize, NIV)
    Queue(elems)
  }

  def apply(elems: Tuple): Queue =
    new Queue(elems)

  def apply(size: Int, meta: Ob, parent: Ob, elems: Tuple): Queue =
    new Queue(elems)

}
