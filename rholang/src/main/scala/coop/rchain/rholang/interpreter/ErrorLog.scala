package coop.rchain.rholang.interpreter

import cats._
import cats.implicits._
import cats.mtl.FunctorTell

class ErrorLog[F[_]: Applicative] extends FunctorTell[F, Throwable] {
  private var errorVector: Vector[Throwable] = Vector.empty
  val functor                                = implicitly[Functor[F]]
  override def tell(e: Throwable): F[Unit] =
    (this
      .synchronized {
        errorVector = errorVector :+ e
      })
      .pure[F]

  override def writer[A](a: A, e: Throwable): F[A] = {
    this.synchronized {
      errorVector = errorVector :+ e
    }; a
  }.pure[F]

  override def tuple[A](ta: (Throwable, A)): F[A] = {
    this.synchronized {
      errorVector = errorVector :+ ta._1
    }; ta._2
  }.pure[F]

  def readAndClearErrorVector(): Vector[Throwable] =
    this.synchronized {
      val ret = errorVector
      errorVector = Vector.empty
      ret
    }
}
