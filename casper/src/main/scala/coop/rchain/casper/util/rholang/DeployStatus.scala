package coop.rchain.casper.util.rholang

import coop.rchain.casper.protocol.DeployData
import coop.rchain.rspace.ReplayException

sealed trait DeployStatus

object DeployStatus {

  def internalError(deploy: DeployData, throwable: Throwable): Failure =
    InvalidDeploy.InternalError(deploy, throwable)

  def replayStatusMismatch(initialFailed: Boolean, replayFailed: Boolean): Failure =
    InvalidDeploy.ReplayStatusMismatch(initialFailed, replayFailed)

  def unusedCOMMEvent(replayException: ReplayException): Failure =
    InvalidDeploy.UnusedCOMMEvent(replayException)

}

sealed trait Failure extends DeployStatus

object Failure {
  final case class UserErrors(errors: Seq[Throwable]) extends Failure
}

sealed trait InvalidDeploy extends Failure

object InvalidDeploy {
  final case class InternalError(deploy: DeployData, throwable: Throwable) extends InvalidDeploy
  final case class ReplayStatusMismatch(initialFailed: Boolean, replayFailed: Boolean)
      extends InvalidDeploy
  final case class UnusedCOMMEvent(replayException: ReplayException) extends InvalidDeploy
}
