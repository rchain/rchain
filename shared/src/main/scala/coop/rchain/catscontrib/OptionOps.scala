package coop.rchain.catscontrib

final class OptionOps[A](self: Option[A]) {
  def toRight[L](left: L): Either[L, A] = self.fold[Either[L, A]](Left(left))(a => Right(a))
}

trait ToOptionOps {
  implicit def ToOptionOpsFromBoolean[A](a: Option[A]): OptionOps[A] = new OptionOps[A](a)
}
