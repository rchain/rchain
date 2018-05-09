package coop.rchain.rholang.interpreter

import cats.MonadError
import monix.eval.{Coeval, Task}
import monix.eval.Task.catsAsync
import cats.implicits._

object errors {
  trait InterpreterError                        extends Throwable
  final case class NormalizerError(msg: String) extends InterpreterError
  final case class SyntaxError(msg: String)     extends InterpreterError
  final case class UnexpectedNameContext(varName: String,
                                         procVarLine: Int,
                                         procVarCol: Int,
                                         nameContextLine: Int,
                                         nameContextCol: Int)
      extends InterpreterError {
    override def toString: String =
      s"Proc variable: $varName at $procVarLine:$procVarCol used in Name context at $nameContextLine:$nameContextCol"
  }

  final case class UnexpectedReuseOfNameContextFree(
      varName: String,
      firstUseLine: Int,
      firstUseCol: Int,
      secondUseLine: Int,
      secondUseCol: Int
  ) extends InterpreterError {
    override def toString: String =
      s"Free variable $varName is used twice as a binder (at $firstUseLine:$firstUseCol and $secondUseLine:$secondUseCol) in name context."
  }

  final case class UnexpectedProcContext(
      varName: String,
      nameVarLine: Int,
      nameVarCol: Int,
      processContextLine: Int,
      processContextCol: Int
  ) extends InterpreterError {
    override def toString: String =
      s"Name variable: $varName at $nameVarLine:$nameVarCol used in process context at $processContextLine:$processContextCol"
  }

  final case class UnexpectedReuseOfProcContextFree(
      varName: String,
      firstUseLine: Int,
      firstUseCol: Int,
      secondUseLine: Int,
      secondUseCol: Int
  ) extends InterpreterError {
    override def toString: String =
      s"Free variable $varName is used twice as a binder (at $firstUseLine:$firstUseCol and $secondUseLine:$secondUseCol) in process context."
  }

  final case class UnexpectedBundleContent(msg: String)               extends InterpreterError
  final case class UnrecognizedNormalizerError(msg: String)           extends InterpreterError
  final case class SubstituteError(msg: String)                       extends InterpreterError
  final case class UnrecognizedInterpreterError(throwable: Throwable) extends InterpreterError
  final case class SortMatchError(msg: String)                        extends InterpreterError
  final case class ReduceError(msg: String)                           extends InterpreterError

  implicit val monadErrorTask: MonadError[Task, InterpreterError] =
    new MonadError[Task, InterpreterError] {
      override def raiseError[A](e: InterpreterError): Task[A] = Task.raiseError(e)

      override def handleErrorWith[A](fa: Task[A])(f: InterpreterError => Task[A]): Task[A] =
        fa.onErrorHandleWith {
          case e: InterpreterError => f(e)
          case other               => Task.raiseError(other)
        }

      override def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => Task[Either[A, B]]): Task[B] =
        Task.tailRecM(a)(f)

      override def pure[A](x: A): Task[A] = Task.pure(x)
    }

  implicit val monadErrorCoeval: MonadError[Coeval, InterpreterError] =
    new MonadError[Coeval, InterpreterError] {
      override def flatMap[A, B](fa: Coeval[A])(f: A => Coeval[B]): Coeval[B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => Coeval[Either[A, B]]): Coeval[B] =
        Coeval.tailRecM(a)(f)

      override def raiseError[A](e: InterpreterError): Coeval[A] =
        Coeval.raiseError(e)

      override def handleErrorWith[A](fa: Coeval[A])(f: InterpreterError => Coeval[A]): Coeval[A] =
        fa.onErrorHandleWith {
          case ie: InterpreterError => f(ie)
          case other                => Coeval.raiseError(other)
        }

      override def pure[A](x: A): Coeval[A] = Coeval.delay(x)
    }

}
