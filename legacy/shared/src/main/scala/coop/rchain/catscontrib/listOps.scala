package coop.rchain.catscontrib

import cats._, cats.data._, cats.syntax.all._
import scala.math.Ordering

object ListContrib {
  def sortBy[A, K: Monoid](list: List[A], map: collection.Map[A, K])(
      implicit ord: Ordering[(K, A)]
  ): List[A] =
    list.sortBy(e => (map.getOrElse(e, Monoid[K].empty), e))(ord)
}
