package coop.rchain.catscontrib

import cats._, cats.data._, cats.syntax.all._

object eitherT extends EitherTInstances

trait EitherTInstances {
  implicit def eitherTMonadTrans[A]: MonadTrans[EitherT[?[_], A, ?]] = new EitherTMonadTrans[A] {}
}

trait EitherTMonadTrans[A] extends MonadTrans[EitherT[?[_], A, ?]] {

  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): EitherT[M, A, B] = EitherT.liftF(mb)

  implicit def apply[M[_]: Monad]: Monad[EitherT[M, A, ?]] = Monad[EitherT[M, A, ?]]
}
