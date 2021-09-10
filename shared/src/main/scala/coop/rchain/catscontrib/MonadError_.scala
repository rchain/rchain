package coop.rchain.catscontrib
import cats.MonadError

trait MonadError_[F[_], E] extends ApplicativeError_[F, E] {
  implicit val instance: MonadError[F, E]
}

object MonadError_ {
  def apply[F[_], E](implicit F: MonadError_[F, E]): MonadError_[F, E] = F

  implicit def fromCatsMonadError[F[_], E](implicit ev: MonadError[F, E]): MonadError_[F, E] =
    new MonadError_[F, E] {
      implicit override val instance: MonadError[F, E] = ev
    }
}
