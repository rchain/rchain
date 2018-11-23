package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._

object stateT extends StateTInstances

trait StateTInstances {
  implicit def stateTMonadTrans[A] = new StateTMonadTrans[A] {}
}

trait StateTMonadTrans[A] extends MonadTrans[StateT[?[_], A, ?]] {

  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): StateT[M, A, B] = StateT.liftF(mb)

  implicit def apply[M[_]: Monad]: Monad[StateT[M, A, ?]] = Monad[StateT[M, A, ?]]
}
