package coop.rchain.catscontrib

import cats._, cats.data._, cats.syntax.all._

object writerT extends WriterTInstances

trait WriterTInstances {
  implicit def writerTMonadTrans[L: Monoid]: MonadTrans[WriterT[*[_], L, *]] =
    new WriterTMonadTrans[L]() {}
}

abstract class WriterTMonadTrans[L: Monoid]() extends MonadTrans[WriterT[*[_], L, *]] {

  def liftM[M[_]: Monad, B](mb: M[B]): WriterT[M, L, B] =
    WriterT.liftF[M, L, B](mb)

  def apply[M[_]: Monad]: Monad[WriterT[M, L, *]] = Monad[WriterT[M, L, *]]
}
