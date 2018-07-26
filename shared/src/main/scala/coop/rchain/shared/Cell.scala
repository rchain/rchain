package coop.rchain.shared

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._
import monix.eval.{MVar, Task}

trait Cell[F[_], S] {
  def modify(f: S => F[S]): F[Unit]
}

object Cell {
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
      }
    }
}

object CellInstances0 {
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
    }

}
