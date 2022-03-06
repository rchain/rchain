package coop.rchain.shared

import cats._
import cats.data.ReaderT
import cats.effect.concurrent.{MVar, MVar2, Ref}
import cats.effect.{Concurrent, ExitCase, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._
import coop.rchain.catscontrib.ski._

trait Cell[F[_], S] {
  def set(s: S): F[Unit] = modify(kp(s))
  def modify(f: S => S): F[Unit]
  def flatModify(f: S => F[S]): F[Unit]
  def read: F[S]
  def reads[A](f: S => A): F[A]
}

object Cell {
  def apply[F[_], S](implicit ev: Cell[F, S]): Cell[F, S] = ev

  def readerT[F[_], E, S](cell: Cell[F, S]): Cell[ReaderT[F, E, *], S] =
    new Cell[ReaderT[F, E, *], S] {
      override def modify(f: S => S): ReaderT[F, E, Unit] = ReaderT.liftF(cell.modify(f))

      override def flatModify(f: S => ReaderT[F, E, S]): ReaderT[F, E, Unit] =
        ReaderT { e =>
          val af: S => F[S] = s => f(s).run(e)
          cell.flatModify(af)
        }

      override def read: ReaderT[F, E, S] = ReaderT.liftF(cell.read)

      override def reads[A](f: S => A): ReaderT[F, E, A] = ReaderT.liftF(cell.reads(f))
    }

  abstract class DefaultCell[F[_]: Functor, S] extends Cell[F, S] {
    override def reads[A](f: S => A) = read map f
  }

  private class MVarCell[F[_], S](mvar: MVar2[F, S])(implicit F: Sync[F])
      extends DefaultCell[F, S] {
    def modify(f: S => S): F[Unit] = flatModify(s => F.pure(f(s)))
    def flatModify(f: S => F[S]): F[Unit] = F.bracketCase(mvar.take)(s => f(s).flatMap(mvar.put)) {
      case (oldState, ExitCase.Error(_) | ExitCase.Canceled) => mvar.put(oldState)
      case _                                                 => F.unit
    }
    def read: F[S] = mvar.read
  }

  private class RefCell[F[_], S](ref: Ref[F, S])(implicit F: Monad[F]) extends DefaultCell[F, S] {
    def modify(f: S => S): F[Unit]        = ref.update(f)
    def flatModify(f: S => F[S]): F[Unit] = ref.get.flatMap(f).flatMap(ref.set)
    def read: F[S]                        = ref.get
  }

  def mvarCell[F[_]: Concurrent, S](initalState: S): F[Cell[F, S]] =
    MVar[F].of(initalState) map (cell => new MVarCell[F, S](cell))

  def refCell[F[_]: Sync, S](initalState: S): F[Cell[F, S]] =
    Ref[F].of(initalState) map (ref => new RefCell[F, S](ref))

  def unsafe[F[_]: Sync, S](initialState: S): Cell[F, S] =
    new RefCell[F, S](Ref.unsafe(initialState))

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  def id[S](init: S): Cell[Id, S] = new DefaultCell[Id, S] {
    var s: S                    = init
    def modify(f: S => S): Unit = flatModify(f)
    def flatModify(f: S => S): Unit =
      s = f(s)
    def read: S = s
  }
}
