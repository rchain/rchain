package coop.rchain.shared

import scala.collection.immutable.Seq

object SeqOps {

  /** Drops the 'i'th element of a list.
    */
  def dropIndex[T](xs: Seq[T], n: Int): Seq[T] = {
    if ((n < 0) || (n >= xs.size)) {
      throw new IndexOutOfBoundsException(s"Index $n is outside the sequence bounds 0..${xs.size}")
    }

    val (l1, l2) = xs splitAt n
    l1 ++ (l2 drop 1)
  }

  /** Removes the first occurrence of an element that matches the given predicate.
    */
  def removeFirst[T](xs: Seq[T])(p: T => Boolean): Seq[T] = {
    val (l1, l2) = xs.span(x => !p(x))
    l1 ++ (l2 drop 1)
  }
}
