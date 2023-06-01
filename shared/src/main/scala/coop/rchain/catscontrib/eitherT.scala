package coop.rchain.catscontrib

import cats._, cats.data._

object eitherT extends EitherTSyntax

trait EitherTSyntax {
  implicit def eitherTMonadTrans[A]: MonadTrans[EitherT[*[_], A, *]] = new EitherTMonadTrans[A] {}
}

trait EitherTMonadTrans[A] extends MonadTrans[EitherT[*[_], A, *]] {

  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): EitherT[M, A, B] = EitherT.liftF(mb)

  def apply[M[_]: Monad]: Monad[EitherT[M, A, *]] = Monad[EitherT[M, A, *]]
}
