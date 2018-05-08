package coop.rchain.catscontrib

import cats.Monoid

import scala.math.Ordering

object ListContrib {
  def sortBy[A, K: Monoid](list: IndexedSeq[A], map: collection.Map[A, K])(
      implicit ord: Ordering[K]) =
    list.sortBy(map.getOrElse(_, Monoid[K].empty))(ord)
}
