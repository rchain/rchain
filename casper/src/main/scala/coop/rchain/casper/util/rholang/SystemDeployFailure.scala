package coop.rchain.casper.util.rholang

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.errors.InterpreterError

sealed trait SystemDeployFailure

sealed abstract class SystemDeployUserError extends SystemDeployFailure

object SystemDeployUserError {
  final case class SystemDeployError(errorMsg: String) extends SystemDeployUserError
  def apply(errorMsg: String) = SystemDeployError(errorMsg)
}

sealed abstract class SystemDeployPlatformFailure extends RuntimeException with SystemDeployFailure

object SystemDeployPlatformFailure {
  final case class UnexpectedResult(result: Seq[Par]) extends SystemDeployPlatformFailure
  final case class UnexpectedSystemErrors(errors: Vector[InterpreterError])
      extends SystemDeployPlatformFailure
  case object ConsumeFailed extends SystemDeployPlatformFailure
}
