package coop.rchain.shared

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._

trait Cell[F[_], S] {
  def modify(f: S => S): F[Unit]
}

object Cell {
  def apply[F[_], S](implicit ev: Cell[F, S]): Cell[F, S] = ev

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans, S](implicit C: Cell[F, S]): Cell[T[F, ?], S] =
    new Cell[T[F, ?], S] {
      def modify(f: S => S): T[F, Unit] = C.modify(f).liftM[T]
    }

  class NOPCell[F[_]: Applicative, S] extends Cell[F, S] {
    def modify(f: S => S): F[Unit] = ().pure[F]
  }
}

object CellInstances0 {
  implicit def eitherTCell[E, F[_]: Monad: Cell[?[_], S], S]: Cell[EitherT[F, E, ?], S] =
    Cell.forTrans[F, EitherT[?[_], E, ?], S]
}
