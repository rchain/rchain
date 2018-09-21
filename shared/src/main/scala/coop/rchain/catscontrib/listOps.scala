package coop.rchain.catscontrib

import cats.{Monad, Monoid}
import cats.implicits._

import scala.math.Ordering

object ListContrib {
  def sortBy[A, K: Monoid](list: IndexedSeq[A], map: collection.Map[A, K])(
      implicit ord: Ordering[K]
  ) =
    list.sortBy(map.getOrElse(_, Monoid[K].empty))(ord)

  // From https://hygt.github.io/2018/08/05/Cats-findM-collectFirstM.html
  def findM[G[_], A](list: List[A], p: A => G[Boolean])(implicit G: Monad[G]): G[Option[A]] =
    list.tailRecM[G, Option[A]] {
      case head :: tail =>
        p(head).map {
          case true  => Right(Some(head))
          case false => Left(tail)
        }
      case Nil => G.pure(Right(None))
    }
}
