package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._

object writerT extends WriterTInstances

trait WriterTInstances {
  implicit def writerTMonadTrans[A: Monoid] = new WriterTMonadTrans[A]() {}
}

abstract class WriterTMonadTrans[L: Monoid]() extends MonadTrans[WriterT[?[_], L, ?]] {

  def liftM[M[_]: Monad, B](mb: M[B]): WriterT[M, L, B] =
    WriterT.liftF[M, L, B](mb)

  implicit def apply[M[_]: Monad]: Monad[WriterT[M, L, ?]] = Monad[WriterT[M, L, ?]]
}
