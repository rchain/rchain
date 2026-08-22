package coop.rchain.rspace.history

import scodec.bits.ByteVector

import scala.annotation.tailrec

final case class KeySegment(value: ByteVector) extends AnyVal {
  def size: Long               = value.size
  def nonEmpty: Boolean        = value.nonEmpty
  def isEmpty: Boolean         = value.isEmpty
  def head: Byte               = value.head
  def tail: KeySegment         = KeySegment(value.tail)
  def headOption: Option[Byte] = value.headOption

  def ++(other: KeySegment): KeySegment =
    KeySegment(value ++ other.value)

  def :+(byte: Byte): KeySegment = KeySegment(value :+ byte)
  def toHex: String              = value.toHex
}

object KeySegment {
  def apply(bv: ByteVector): KeySegment = {
    require(bv.size <= 127, "Size of key segment is more than 127")
    new KeySegment(bv)
  }
  def apply(ab: Array[Byte]): KeySegment = KeySegment(ByteVector(ab))
  def apply(sb: Seq[Byte]): KeySegment   = KeySegment(ByteVector(sb))
  val empty: KeySegment                  = KeySegment(ByteVector.empty)

  /**
    * Find the common part of a and b.
    *
    * @return (Common part, rest of a, rest of b).
    */
  def commonPrefix(a: KeySegment, b: KeySegment): (KeySegment, KeySegment, KeySegment) = {
    @tailrec
    def go(common: KeySegment, l: KeySegment, r: KeySegment): (KeySegment, KeySegment, KeySegment) =
      if (r.isEmpty || l.isEmpty) (common, l, r)
      else {
        val lHead = l.head
        val rHead = r.head
        if (lHead == rHead) go(common :+ lHead, l.tail, r.tail)
        else (common, l, r)
      }

    go(KeySegment.empty, a, b)
  }
}
