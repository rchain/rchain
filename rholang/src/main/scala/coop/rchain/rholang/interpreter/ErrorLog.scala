package coop.rchain.rholang.interpreter

import cats._
import cats.effect._
import cats.mtl.FunctorTell
import coop.rchain.rholang.interpreter.errors.InterpreterError

@SuppressWarnings(Array("org.wartremover.warts.Var"))
// TODO remove/modify this monster!
class ErrorLog[F[_]: Sync] extends FunctorTell[F, InterpreterError] {
  private var errorVector: Vector[InterpreterError] = Vector.empty
  val functor                                       = implicitly[Functor[F]]
  override def tell(e: InterpreterError): F[Unit] = Sync[F].delay {
    this.synchronized {
      errorVector = errorVector :+ e
    }
  }

  override def writer[A](a: A, e: InterpreterError): F[A] = Sync[F].delay {
    this.synchronized {
      errorVector = errorVector :+ e
    }; a
  }

  override def tuple[A](ta: (InterpreterError, A)): F[A] = Sync[F].delay {
    this.synchronized {
      errorVector = errorVector :+ ta._1
    }; ta._2
  }

  def readAndClearErrorVector(): F[Vector[InterpreterError]] = Sync[F].delay {
    this.synchronized {
      val ret = errorVector
      errorVector = Vector.empty
      ret
    }
  }
}
