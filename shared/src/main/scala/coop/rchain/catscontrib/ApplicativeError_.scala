package coop.rchain.catscontrib

import cats._

trait ApplicativeError_[F[_], E] {

  implicit val instance: ApplicativeError[F, E]

  def raiseError[A](e: E): F[A] = instance.raiseError(e)
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
