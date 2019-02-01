package coop.rchain.rholang.interpreter

object errors {

  sealed abstract class InterpreterError(message: String) extends Throwable(message) {

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

}
