package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.compiler.SourcePosition
import net.logstash.logback.encoder.org.apache.commons.lang3.exception.ExceptionUtils

object errors {

  sealed abstract class InterpreterError(message: String) extends Throwable(message) {

    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
    def this(message: String, cause: Throwable) {
      this(message)
      initCause(cause)
    }
  }

  final case class BugFoundError(message: String)   extends InterpreterError(message)
  final case class NormalizerError(message: String) extends InterpreterError(message)
  final case class SyntaxError(message: String)     extends InterpreterError(message)
  final case class LexerError(message: String)      extends InterpreterError(message)
  final case class ParserError(message: String)     extends InterpreterError(message)

  final case class UnboundVariableRef(varName: String, line: Int, col: Int)
      extends InterpreterError(s"Variable reference: =$varName at $line:$col is unbound.")

  final case class UnexpectedNameContext(
      varName: String,
      procVarSourcePosition: SourcePosition,
      nameSourcePosition: SourcePosition
  ) extends InterpreterError(
        s"Proc variable: $varName at $procVarSourcePosition used in Name context at $nameSourcePosition"
      )

  final case class UnexpectedReuseOfNameContextFree(
      varName: String,
      firstUse: SourcePosition,
      secondUse: SourcePosition
  ) extends InterpreterError(
        s"Free variable $varName is used twice as a binder " +
          s"(at $firstUse and $secondUse) in name context."
      )

  final case class UnexpectedProcContext(
      varName: String,
      nameVarSourcePosition: SourcePosition,
      processSourcePosition: SourcePosition
  ) extends InterpreterError(
        s"Name variable: $varName at $nameVarSourcePosition " +
          s"used in process context at $processSourcePosition"
      )

  final case class UnexpectedReuseOfProcContextFree(
      varName: String,
      firstUse: SourcePosition,
      secondUse: SourcePosition
  ) extends InterpreterError(
        s"Free variable $varName is used twice as a binder " +
          s"(at $firstUse and $secondUse) in process context."
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

  // Errors from parallel execution
  final case class AggregateError(
      interpreterErrors: Vector[InterpreterError],
      errors: Vector[Throwable]
  ) extends InterpreterError(
        s"Error: Aggregate Error\n${(interpreterErrors ++ errors).map(ExceptionUtils.getStackTrace).mkString}"
      )

  // Current implementation of SpaceMatcher (extractDataCandidates) causes unmatched comms
  // with the same binding channels and overlapping patterns.
  // Temporarily these kind of joins `for (<- @2 & <- @2)` are not allowed.
  // https://rchain.atlassian.net/browse/RCHAIN-4032
  final case class ReceiveOnSameChannelsError(line: Int, col: Int)
      extends InterpreterError(
        s"Receiving on the same channels is currently not allowed (at $line:$col). Ref. RCHAIN-4032."
      )
}
