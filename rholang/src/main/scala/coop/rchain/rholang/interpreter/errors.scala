package coop.rchain.rholang.interpreter

import cats.{Monad, MonadError}
import monix.eval.{Coeval, Task}
import monix.eval.Task.catsAsync
import cats.implicits._
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.rholang.implicits._

object errors {
  type InterpreterErrorsM[M[_]] = MonadError[M, InterpreterError]
  def interpreterErrorM[M[_]: Monad: InterpreterErrorsM] = MonadError[M, InterpreterError]

  trait InterpreterError                                          extends Throwable
  final case class NormalizerError(override val toString: String) extends InterpreterError
  final case class SyntaxError(override val toString: String)     extends InterpreterError
  final case class UnboundVariableRef(varName: String, line: Int, col: Int)
      extends InterpreterError {
    override def toString: String =
      s"Variable reference: =$varName at $line:$col is unbound."
  }
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

  final case class UnexpectedBundleContent(override val toString: String) extends InterpreterError
  final case class UnrecognizedNormalizerError(override val toString: String)
      extends InterpreterError
  final case class TopLevelWildcardsNotAllowedError(wildcards: String) extends InterpreterError {
    override def toString: String =
      s"Top level wildcards are not allowed: $wildcards."
  }
  final case class TopLevelFreeVariablesNotAllowedError(freeVars: String) extends InterpreterError {
    override def toString: String =
      s"Top level free variables are not allowed: $freeVars."
  }
  final case class SubstituteError(override val toString: String)     extends InterpreterError
  final case class UnrecognizedInterpreterError(throwable: Throwable) extends InterpreterError
  final case class SortMatchError(override val toString: String)      extends InterpreterError
  final case class ReduceError(override val toString: String)         extends InterpreterError
  final case class MethodNotDefined(method: String, otherType: String) extends InterpreterError {
    override def toString: String =
      s"Error: Method `$method` is not defined on $otherType."
  }
  final case class MethodArgumentNumberMismatch(
      method: String,
      expected: Int,
      actual: Int
  ) extends InterpreterError {
    override def toString: String =
      s"Error: Method `$method` expects $expected Par argument(s), but got $actual argument(s)."
  }
  final case class OperatorNotDefined(op: String, otherType: String) extends InterpreterError {
    override def toString: String =
      s"Error: Operator `$op` is not defined on $otherType."
  }
  final case class OperatorExpectedError(
      op: String,
      expected: String,
      otherType: String
  ) extends InterpreterError {
    override def toString: String =
      s"Error: Operator `$op` expected $expected but got $otherType."
  }

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
