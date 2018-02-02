package coop.rchain.rosette

import java.util.NoSuchElementException

case class Queue(var elems: Tuple) extends Ob {

  def head: Ob = elems.elem.head

  def tail: Ob = elems.elem.last

  def nElem: Ob = Fixnum(elems.elem.length)

  def depth(): Int =
    elems.elem.length

  def empty(): Boolean =
    (nElem.asInstanceOf[Fixnum] == Fixnum(0)).value

  def enQueue(ele: Ob): Unit =
    elems = Tuple(elems.elem :+ ele)

  def deQueue(): Ob =
    if (empty()) {
      throw new NoSuchElementException("queue empty")
    } else {
      val v = elems.elem.head
      elems = Tuple(elems.elem.drop(1))
      v
    }

  def reset(): Unit =
    elems = Tuple.Placeholder

  override def indexedSize: Ob = nElem

  override def nth(i: Int): Option[Ob] =
    elems.elem.lift(i)

  override def setNth(i: Int, v: Ob): Option[Ob] =
    if (empty()) {
      None
    } else {
      elems = Tuple(elems.elem.updated(i, v))
      Some(this)
    }

  override def subObject(i1: Int, i2: Int): Ob = {
    val newElems = elems.makeSlice(i1, i2)
    val newQueue = Queue.create()
    for (ob <- newElems.elem) {
      newQueue.enQueue(ob)
    }
    newQueue
  }

  def patternDequeue(pat: Tuple): Option[Ob] = {
    val len = nElem.asInstanceOf[Fixnum].value
    for((e,i) <- elems.elem.zipWithIndex) {
      e match {
        case msg: Tuple =>
          if(pat.matches(msg)){
            val newElems = elems.elem.slice(0,i) ++: elems.elem.slice(i+1,len)
            elems = Tuple(newElems)
            return Some(e)
          }
        case _  =>
      }
    }
     None
  }

  def patternRead(pat: Tuple): Option[Ob] = {
    for((e,i) <- elems.elem.zipWithIndex) {
      e match {
        case msg: Tuple =>
          if(pat.matches(msg)){
            return Some(e)
          }
        case _  =>
      }
    }
    None
  }

  def dequeueNth(i: Int): Option[Ob] = {
    val len = nElem.asInstanceOf[Fixnum].value
    if(i < 0 || i > len) {
      return None
    }
    val msg = elems.elem(i)
    val newElems = elems.elem.slice(0,i) ++: elems.elem.slice(i+1,len)
    elems = Tuple(newElems)
    Some(msg)
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
