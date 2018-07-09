package coop.rchain.rholang.interpreter

import cats.Functor
import cats.mtl.FunctorTell
import coop.rchain.rholang.interpreter.errors.InterpreterError
import monix.eval.Task

class ErrorLog extends FunctorTell[Task, InterpreterError] {
  private var errorVector: Vector[InterpreterError] = Vector.empty
  val functor                                       = implicitly[Functor[Task]]
  override def tell(e: InterpreterError): Task[Unit] =
    Task.now {
      this.synchronized {
        errorVector = errorVector :+ e
      }
    }

  override def writer[A](a: A, e: InterpreterError): Task[A] =
    Task.now {
      this.synchronized {
        errorVector = errorVector :+ e
      }
      a
    }

  override def tuple[A](ta: (InterpreterError, A)): Task[A] =
    Task.now {
      this.synchronized {
        errorVector = errorVector :+ ta._1
      }
      ta._2
    }

  def readAndClearErrorVector(): Vector[InterpreterError] =
    this.synchronized {
      val ret = errorVector
      errorVector = Vector.empty
      ret
    }
}
