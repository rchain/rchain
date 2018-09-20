package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._

/** Like ApplicativeError but not Applicative thus no ambigous conflicts */
/** We should REALLY use Scalaz */
trait ApplicativeError_[F[_], E] {

  def raiseError[A](e: E): F[A]
  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

  def fromEither[A](x: E Either A)(implicit ev: Applicative[F]): F[A] =
    x.fold(raiseError, Applicative[F].pure)

  def attempt[A](fa: F[A])(implicit ev: Applicative[F]): F[Either[E, A]] =
    handleErrorWith(
      Applicative[F].map(fa)(Right(_): Either[E, A])
    )(e => Applicative[F].pure(Left(e)))

}

object ApplicativeError_ extends ApplicativeError_Instances {
  def apply[F[_], E](implicit F: ApplicativeError_[F, E]): ApplicativeError_[F, E] = F
}

trait ApplicativeError_Instances {

  implicit def applicativeError[F[_], E](
      implicit F: ApplicativeError[F, E]
  ): ApplicativeError_[F, E] = new ApplicativeError_[F, E] {
    def raiseError[A](e: E): F[A] = ApplicativeError[F, E].raiseError(e)
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A] =
      ApplicativeError[F, E].handleErrorWith(fa)(f)
  }

}
