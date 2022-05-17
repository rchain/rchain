package coop.rchain.rspace.history
import coop.rchain.rspace.syntax._

import scala.annotation.tailrec

/**
  * Segment of a History key
  * @tparam V type of a key value
  * @tparam W type of a word in key alphabet
  */
trait KeySegment[K, W] {
  def empty: K
  def size(ks: K): Int
  def isEmpty(ks: K): Boolean
  def headOption(ks: K): Option[W]
  def tailOption(ks: K): Option[K]
  def :+(k: K, w: W): K
  def ++(k1: K, k2: K): K
}

object KeySegment {

  /**
    * Find the common part of b1 and b2.
    * @return (Common part, rest of b1, rest of b2).
    */
  def commonPrefix[K, W](a: K, b: K)(implicit ks: KeySegment[K, W]): (K, K, K) = {
    @tailrec
    def go(common: K, l: K, r: K): (K, K, K) =
      if (r.isEmpty || l.isEmpty) (common, l, r)
      else {
        val lHead = l.head
        val rHead = r.head
        if (lHead == rHead) go(common :+ lHead, l.tail, r.tail)
        else (common, l, r)
      }

    go(ks.empty, a, b)
  }
}
