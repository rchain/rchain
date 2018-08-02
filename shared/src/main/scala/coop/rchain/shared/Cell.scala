package coop.rchain.shared

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._
import monix.eval.{MVar, Task}

trait Cell[F[_], S] {
  def modify(f: S => F[S]): F[Unit]
  def read: F[S]
}

object Cell extends CellInstances0 {
  def apply[F[_], S](implicit ev: Cell[F, S]): Cell[F, S] = ev

  def mvarCell[S](initalState: S): Task[Cell[Task, S]] =
    MVar(initalState) map { mvar =>
      new Cell[Task, S] {
        def modify(f: S => Task[S]): Task[Unit] =
          for {
            s  <- mvar.take
            ns <- f(s)
            _  <- mvar.put(ns)
          } yield ()

        def read: Task[S] = mvar.read
      }
    }

  def const[F[_]: Applicative, S](const: S): Cell[F, S] = new Cell[F, S] {
    def modify(f: S => F[S]): F[Unit] = ().pure[F]
    def read: F[S]                    = const.pure[F]
  }
}

trait CellInstances0 {
  implicit def eitherTCell[E, F[_]: Monad, S](
      implicit
      fCell: Cell[F, S]): Cell[EitherT[F, E, ?], S] =
    new Cell[EitherT[F, E, ?], S] {
      def modify(f: S => EitherT[F, E, S]): EitherT[F, E, Unit] =
        EitherT(
          fCell
            .modify(s =>
              f(s).value >>= {
                case Right(ns) => ns.pure[F]
                case Left(_)   => s.pure[F]
            })
            .map(Right(_).leftCast[E]))

      def read: EitherT[F, E, S] = EitherT(fCell.read.map(Right(_).leftCast[E]))
    }

}
