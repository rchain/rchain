package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._

final class ApplicativeError_Ops[F[_], E, A](self: F[A])(
    implicit err: ApplicativeError_[F, E],
    ev: Applicative[F]
) {
  def attempt: F[Either[E, A]] = err.attempt(self)
}

trait ToApplicativeError_Ops {
  implicit def ToApplicativeError_Ops[F[_], E, A](
      fa: F[A]
  )(implicit err: ApplicativeError_[F, E], ev: Applicative[F]): ApplicativeError_Ops[F, E, A] =
    new ApplicativeError_Ops[F, E, A](fa)
}
