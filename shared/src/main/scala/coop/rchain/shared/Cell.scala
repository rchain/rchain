package coop.rchain.shared

import cats._
import cats.effect.concurrent.{MVar, Ref}
import cats.effect.{Concurrent, Sync}
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import coop.rchain.catscontrib.ski._

trait Cell[F[_], S] {
  def set(s: S): F[Unit] = modify(kp(s))
  def modify(f: S => S): F[Unit]
  def flatModify(f: S => F[S]): F[Unit]
  def read: F[S]
}

object Cell {
  def apply[F[_], S](implicit ev: Cell[F, S]): Cell[F, S] = ev

  def mvarCell[F[_]: Concurrent, S](initalState: S): F[Cell[F, S]] =
    MVar[F].of(initalState) map { mvar =>
      new Cell[F, S] {
        def modify(f: S => S): F[Unit] =
          for {
            s <- mvar.take
            _ <- mvar.put(f(s))
          } yield ()

        def flatModify(f: S => F[S]): F[Unit] =
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

  def refCell[F[_]: Sync, S](initalState: S): F[Cell[F, S]] =
    Ref[F].of(initalState) map { ref =>
      new Cell[F, S] {
        def modify(f: S => S): F[Unit] = ref.modify { in =>
          (f(in), ())
        }

        def flatModify(f: S => F[S]): F[Unit] =
          for {
            s <- ref.get
            _ <- f(s).attempt.flatMap {
                  case Left(e)   => e.raiseError[F, Unit]
                  case Right(ns) => ref.set(ns)
                }
          } yield ()

        def read: F[S] = ref.get
      }
    }

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def unsafe[F[_]: Applicative, S](const: S): Cell[F, S] =
    new Cell[F, S] {
      private var s: S               = const
      def modify(f: S => S): F[Unit] = { s = f(s) }.pure[F]
      def flatModify(f: S => F[S]): F[Unit] = f(s).map { newS =>
        s = newS
        ()
      }
      def read: F[S] = s.pure[F]
    }

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def id[S](init: S): Cell[Id, S] = new Cell[Id, S] {
    var s: S                    = init
    def modify(f: S => S): Unit = flatModify(f)
    def flatModify(f: S => S): Unit =
      s = f(s)
    def read: S = s
  }
}
