package coop.rchain.catscontrib

import cats._

/** Like ApplicativeError but not Applicative thus no ambigous conflicts */
/** We should REALLY use Scalaz */
trait ApplicativeError_[F[_], E] {

  implicit val instance: ApplicativeError[F, E]

  def raiseError[A](e: E): F[A]                        = instance.raiseError(e)
  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A] = instance.handleErrorWith(fa)(f)
  def fromEither[A](x: E Either A): F[A]               = instance.fromEither(x)
  def attempt[A](fa: F[A]): F[Either[E, A]]            = instance.attempt(fa)

}

object ApplicativeError_ extends ApplicativeError_Instances {
  def apply[F[_], E](implicit F: ApplicativeError_[F, E]): ApplicativeError_[F, E] = F
}

trait ApplicativeError_Instances {

  implicit def applicativeError[F[_], E](
      implicit F: ApplicativeError[F, E]
  ): ApplicativeError_[F, E] = new ApplicativeError_[F, E] {
    val instance = F
  }

}
