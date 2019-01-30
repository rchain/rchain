package coop.rchain.rholang.interpreter

/**
  * This is a lawless typeclass to abstract over types providing a `takeFirst()` method, as defined in
  * NonDetFreeMapWithCost's ops.
  *
  * @tparam F
  */
trait Splittable[F[_]] {

  def takeFirst[A](fa: F[A]): F[A]

}

object Splittable {

  def apply[F[_]](implicit ev: Splittable[F]) = ev

  implicit class SplittableOps[F[_]: Splittable, A](self: F[A]) {

    def takeFirst(): F[A] =
      Splittable[F].takeFirst[A](self)
  }

}
