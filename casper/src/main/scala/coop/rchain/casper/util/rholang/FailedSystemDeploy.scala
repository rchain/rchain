package coop.rchain.casper.util.rholang

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.errors.InterpreterError

sealed trait FailedSystemDeploy {
  def systemDeployId: String
}

object FailedSystemDeploy {
  final case class DeployError(override val systemDeployId: String, error: String)
      extends FailedSystemDeploy
  final case class ConsumeError(override val systemDeployId: String, result: Par)
      extends FailedSystemDeploy
  final case class EvalError(override val systemDeployId: String, errors: Vector[InterpreterError])
      extends FailedSystemDeploy
}
