package coop.rchain.catscontrib

import cats._, cats.data._, cats.syntax.all._

object stateT extends StateTInstances

trait StateTInstances {
  implicit def stateTMonadTrans[A]: MonadTrans[StateT[*[_], A, *]] = new StateTMonadTrans[A] {}
}

trait StateTMonadTrans[A] extends MonadTrans[StateT[*[_], A, *]] {

  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): StateT[M, A, B] = StateT.liftF(mb)

  def apply[M[_]: Monad]: Monad[StateT[M, A, *]] = Monad[StateT[M, A, *]]
}
