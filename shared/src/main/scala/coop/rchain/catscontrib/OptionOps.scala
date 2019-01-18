package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._

final class OptionOps[A](self: Option[A]) {
  def toRight[L](left: L): Either[L, A] = self.fold[Either[L, A]](left.asLeft[A])(a => a.asRight[L])
}

trait ToOptionOps {
  implicit def ToOptionOpsFromBoolean[A](a: Option[A]): OptionOps[A] = new OptionOps[A](a)
}
