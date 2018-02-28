package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._

/** Like ApplicativeError but not Applicative thus no ambigous conflicts */
/** We should REALLY use Scalaz */
trait ApplicativeError_[F[_], E] {

  def raiseError[A](e: E): F[A]

  def fromEither[A](x: E Either A)(implicit ev: Applicative[F]): F[A] =
    x.fold(raiseError, Applicative[F].pure)
}

object ApplicativeError_ extends ApplicativeError_Instances {
  def apply[F[_], E](implicit F: ApplicativeError_[F, E]): ApplicativeError_[F, E] = F
}

trait ApplicativeError_Instances {

  implicit def applicativeError[F[_], E](
      implicit F: ApplicativeError[F, E]): ApplicativeError_[F, E] = new ApplicativeError_[F, E] {
    def raiseError[A](e: E): F[A] = ApplicativeError[F, E].raiseError(e)
  }

}
