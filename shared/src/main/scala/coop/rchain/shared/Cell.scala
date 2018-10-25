package coop.rchain.shared

import cats._
import cats.data._
import cats.effect.Concurrent
import cats.effect.concurrent.MVar
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.either._

trait Cell[F[_], S] {
  def modify(f: S => F[S]): F[Unit]
  def read: F[S]
}

object Cell extends CellInstances0 {
  def apply[F[_], S](implicit ev: Cell[F, S]): Cell[F, S] = ev

  def mvarCell[F[_]: Concurrent, S](initalState: S): F[Cell[F, S]] =
    MVar[F].of(initalState) map { mvar =>
      new Cell[F, S] {
        def modify(f: S => F[S]): F[Unit] =
          for {
            s <- mvar.take
            _ <- f(s).attempt.flatMap {
                  case Left(e)   => mvar.put(s).flatMap(_ => e.raiseError[F, Unit])
                  case Right(ns) => mvar.put(ns)
                }
          } yield ()

        def read: F[S] = mvar.read
      }
    }

  def unsafe[F[_]: Applicative, S](const: S): Cell[F, S] =
    new Cell[F, S] {
      private var s: S = const
      def modify(f: S => F[S]): F[Unit] = f(s).map { newS =>
        s = newS
        ()
      }
      def read: F[S] = s.pure[F]
    }

  def id[S](init: S): Cell[Id, S] = new Cell[Id, S] {
    var s: S = init
    def modify(f: S => S): Unit =
      s = f(s)
    def read: S = s
  }
}

trait CellInstances0 {
  implicit def eitherTCell[E, F[_]: Monad, S](
      implicit
      fCell: Cell[F, S]
  ): Cell[EitherT[F, E, ?], S] =
    new Cell[EitherT[F, E, ?], S] {
      def modify(f: S => EitherT[F, E, S]): EitherT[F, E, Unit] =
        EitherT(
          fCell
            .modify(
              s =>
                f(s).value >>= {
                  case Right(ns) => ns.pure[F]
                  case Left(_)   => s.pure[F]
                }
            )
            .map(Right(_).leftCast[E])
        )

      def read: EitherT[F, E, S] = EitherT(fCell.read.map(Right(_).leftCast[E]))
    }

}
