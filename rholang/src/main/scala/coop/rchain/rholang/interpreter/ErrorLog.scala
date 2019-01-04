package coop.rchain.rholang.interpreter

import cats._
import cats.effect._
import cats.implicits._
import cats.mtl.FunctorTell

class ErrorLog[F[_]: Sync] extends FunctorTell[F, Throwable] {
  private var errorVector: Vector[Throwable] = Vector.empty
  val functor                                = implicitly[Functor[F]]
  override def tell(e: Throwable): F[Unit] = Sync[F].delay {
    this.synchronized {
      errorVector = errorVector :+ e
    }
  }

  override def writer[A](a: A, e: Throwable): F[A] = Sync[F].delay {
    this.synchronized {
      errorVector = errorVector :+ e
    }; a
  }

  override def tuple[A](ta: (Throwable, A)): F[A] = Sync[F].delay {
    this.synchronized {
      errorVector = errorVector :+ ta._1
    }; ta._2
  }

  def readAndClearErrorVector(): F[Vector[Throwable]] = Sync[F].delay {
    this.synchronized {
      val ret = errorVector
      errorVector = Vector.empty
      ret
    }
  }
}
