package coop.rchain.catscontrib

import scala.concurrent.Future

final class FuturableOps[F[_]: Futurable, A](fa: F[A]) {
  def toFuture: Future[A] = Futurable[F].toFuture(fa)
}

trait ToFuturableOps {
  implicit def ToFuturableOps[F[_]: Futurable, A](fa: F[A]): FuturableOps[F, A] =
    new FuturableOps[F, A](fa)
}
