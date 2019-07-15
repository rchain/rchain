package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._
import scala.math.Ordering

object ListContrib {
  def sortBy[A, K: Monoid](list: List[A], map: collection.Map[A, K])(
      implicit ord: Ordering[(K, A)]
  ): List[A] =
    list.sortBy(e => (map.getOrElse(e, Monoid[K].empty), e))(ord)

  // From https://hygt.github.io/2018/08/05/Cats-findM-collectFirstM.html
  def findM[G[_], A](list: List[A], p: A => G[Boolean])(implicit G: Monad[G]): G[Option[A]] =
    list.tailRecM[G, Option[A]] {
      case head :: tail =>
        p(head).map {
          case true  => Some(head).asRight[List[A]]
          case false => tail.asLeft[Option[A]]
        }
      case Nil => G.pure(None.asRight[List[A]])
    }
}
