package coop.rchain.catscontrib

import cats._, cats.data._, cats.implicits._

object stateT extends StateTInstances

trait StateTInstances {
  implicit def monadTrans[A] = new StateTMonadTrans[A] {}
}

trait StateTMonadTrans[S] extends MonadTrans[StateT[?[_], S, ?]] {

  def liftM[M[_], A](mb: M[A])(implicit M: Monad[M]): StateT[M, S, A] =
    StateT.liftF(mb)

  implicit def apply[M[_]: Monad]: Monad[StateT[M, S, ?]] = Monad[StateT[M, S, ?]]
}
