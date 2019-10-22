package coop.rchain.casper.util.rholang

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.errors.InterpreterError

sealed trait SystemDeployFailure

object SystemDeployFailure {
  final case class DeployError(error: String)                         extends SystemDeployFailure
  final case class UnexpectedResult(result: Seq[Par])                 extends SystemDeployFailure
  final case class UnexpectedErrors(errors: Vector[InterpreterError]) extends SystemDeployFailure
  case object ConsumeFailed                                           extends SystemDeployFailure
}
