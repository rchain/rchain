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

  sealed abstract class InterpreterError(message: String) extends Throwable(message) {

    def this(message: String, cause: Throwable) {
      this(message)
      initCause(cause)
    }
  }

  final case class NormalizerError(message: String) extends InterpreterError(message)
  final case class SyntaxError(message: String)     extends InterpreterError(message)
  final case class LexerError(message: String)      extends InterpreterError(message)

  final case class UnboundVariableRef(varName: String, line: Int, col: Int)
      extends InterpreterError(s"Variable reference: =$varName at $line:$col is unbound.")

  final case class UnexpectedNameContext(
      varName: String,
      procVarLine: Int,
      procVarCol: Int,
      nameContextLine: Int,
      nameContextCol: Int
  ) extends InterpreterError(
        s"Proc variable: $varName at $procVarLine:$procVarCol used in Name context at $nameContextLine:$nameContextCol"
      )

  final case class UnexpectedReuseOfNameContextFree(
      varName: String,
      firstUseLine: Int,
      firstUseCol: Int,
      secondUseLine: Int,
      secondUseCol: Int
  ) extends InterpreterError(
        s"Free variable $varName is used twice as a binder " +
          s"(at $firstUseLine:$firstUseCol and $secondUseLine:$secondUseCol) in name context."
      )

  final case class UnexpectedProcContext(
      varName: String,
      nameVarLine: Int,
      nameVarCol: Int,
      processContextLine: Int,
      processContextCol: Int
  ) extends InterpreterError(
        s"Name variable: $varName at $nameVarLine:$nameVarCol " +
          s"used in process context at $processContextLine:$processContextCol"
      )

  final case class UnexpectedReuseOfProcContextFree(
      varName: String,
      firstUseLine: Int,
      firstUseCol: Int,
      secondUseLine: Int,
      secondUseCol: Int
  ) extends InterpreterError(
        s"Free variable $varName is used twice as a binder " +
          s"(at $firstUseLine:$firstUseCol and $secondUseLine:$secondUseCol) in process context."
      )

  final case class UnexpectedBundleContent(message: String)     extends InterpreterError(message)
  final case class UnrecognizedNormalizerError(message: String) extends InterpreterError(message)

  final case object OutOfPhlogistonsError
      extends InterpreterError("Computation ran out of phlogistons.")

  final case class TopLevelWildcardsNotAllowedError(wildcards: String)
      extends InterpreterError(s"Top level wildcards are not allowed: $wildcards.")

  final case class TopLevelFreeVariablesNotAllowedError(freeVars: String)
      extends InterpreterError(s"Top level free variables are not allowed: $freeVars.")

  final case class TopLevelLogicalConnectivesNotAllowedError(connectives: String)
      extends InterpreterError(s"Top level logical connectives are not allowed: $connectives.")

  final case class SubstituteError(message: String) extends InterpreterError(message)

  final case class PatternReceiveError(connectives: String)
      extends InterpreterError(
        s"Invalid pattern in the receive: $connectives. Only logical AND is allowed."
      )

  final case class SetupError(message: String) extends InterpreterError(message)

  final case class UnrecognizedInterpreterError(throwable: Throwable)
      extends InterpreterError("Unrecognized interpreter error", throwable)

  final case class SortMatchError(message: String) extends InterpreterError(message)
  final case class ReduceError(message: String)    extends InterpreterError(message)

  final case class MethodNotDefined(method: String, otherType: String)
      extends InterpreterError(s"Error: Method `$method` is not defined on $otherType.")

  final case class MethodArgumentNumberMismatch(method: String, expected: Int, actual: Int)
      extends InterpreterError(
        s"Error: Method `$method` expects $expected Par argument(s), but got $actual argument(s)."
      )

  final case class OperatorNotDefined(op: String, otherType: String)
      extends InterpreterError(s"Error: Operator `$op` is not defined on $otherType.")

  final case class OperatorExpectedError(op: String, expected: String, otherType: String)
      extends InterpreterError(s"Error: Operator `$op` is not defined on $otherType.")

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
