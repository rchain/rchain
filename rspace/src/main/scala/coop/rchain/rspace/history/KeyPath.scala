package coop.rchain.rspace.history

import scodec.bits.ByteVector

sealed trait KeyPath {
  def value: ByteVector
  def size: Long                  = value.size
  def nonEmpty: Boolean           = value.nonEmpty
  def head: Byte                  = value.head
  def tailToValue: ByteVector     = value.tail
  def tail: KeyPath               = KeyPath(tailToValue)
  def ++(other: KeyPath): KeyPath = KeyPath(value ++ other.value)
  def ==(other: KeyPath): Boolean = value == other.value
  def headOption: Option[Byte] =
    if (value.isEmpty) None else Option(value.head)
}
// TODO: Rename KeyPathNew to KeyPath (after removing implementation [[History.KeyPath]])
object KeyPath {
  def create(bv: ByteVector): KeyPath =
    new KeyPath {
      override def value: ByteVector = bv
    }
  def equal(a: KeyPath, b: KeyPath): Boolean = a.value == b.value
  def commonPath(a: KeyPath, b: KeyPath): KeyPath = {
    val commonPart = (a.value.toArray, b.value.toArray).zipped.takeWhile {
      case (ll, rr) => ll == rr
    }
    val commonPartBV = ByteVector(commonPart.map(_._1).toArray)
    KeyPath.create(commonPartBV)
  }

  def apply(sb: Seq[Byte]): KeyPath =
    KeyPath.create(ByteVector(sb.toArray))

  def apply(bv: ByteVector): KeyPath =
    KeyPath.create(bv)

  def apply(ab: Array[Byte]): KeyPath =
    KeyPath.create(ByteVector(ab))

  val empty = KeyPath.create(ByteVector.empty)
}
