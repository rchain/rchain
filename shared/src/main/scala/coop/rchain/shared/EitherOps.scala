package coop.rchain.shared

object EitherOps {
  def zip[A, B, C](x: Either[A, B], y: Either[A, C]): Either[A, (B, C)] =
    for {
      a <- x
      b <- y
    } yield (a, b)
}
