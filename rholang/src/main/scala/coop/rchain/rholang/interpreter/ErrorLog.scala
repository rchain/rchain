package coop.rchain.rholang.interpreter

import cats.Functor
import cats.mtl.FunctorTell
import monix.eval.Task

class ErrorLog extends FunctorTell[Task, Throwable] {
  private var errorVector: Vector[Throwable] = Vector.empty
  val functor                                = implicitly[Functor[Task]]
  override def tell(e: Throwable): Task[Unit] =
    Task.now {
      this.synchronized {
        errorVector = errorVector :+ e
      }
    }

  override def writer[A](a: A, e: Throwable): Task[A] =
    Task.now {
      this.synchronized {
        errorVector = errorVector :+ e
      }
      a
    }

  override def tuple[A](ta: (Throwable, A)): Task[A] =
    Task.now {
      this.synchronized {
        errorVector = errorVector :+ ta._1
      }
      ta._2
    }

  def readAndClearErrorVector(): Vector[Throwable] =
    this.synchronized {
      val ret = errorVector
      errorVector = Vector.empty
      ret
    }
}
