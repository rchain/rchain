package coop.rchain.rholang.interpreter

import cats.Functor
import cats.implicits._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.mtl.FunctorTell

class ErrorLog[F[_] : Sync] (private val errors : Ref[F, Vector[Throwable]]) extends FunctorTell[F, Throwable] {
  override  val functor: Functor[F] = implicitly[Functor[F]]

  override def tell(e: Throwable): F[Unit] = errors.update( es => es :+ e )

  override def writer[A](a: A, e: Throwable): F[A] = tell(e) *> a.pure[F]

  override def tuple[A](ta: (Throwable, A)): F[A] = tell(ta._1) *> ta._2.pure[F]

  def readAndClearErrorVector(): F[Vector[Throwable]] =
    for {
      previousErrors <- errors.get
      _ <- errors.set(Vector.empty)
    } yield previousErrors
}

object ErrorLog {
  def create[F[_] : Sync]: F[ErrorLog[F]] =
    Ref.of[F, Vector[Throwable]](Vector.empty[Throwable]).map(new ErrorLog(_))
}