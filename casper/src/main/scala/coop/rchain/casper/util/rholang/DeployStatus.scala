package coop.rchain.casper.util.rholang

import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.rspace.ReplayException

sealed trait DeployStatus { self =>
  def isFailed: Boolean = self match {
    case _: Failed => true
    case _         => false
  }

  def isInternalError: Boolean = self match {
    case _: InternalError => true
    case _                => false
  }
}

case object Succeeded                                                           extends DeployStatus
sealed trait Failed                                                             extends DeployStatus
final case class UnusedCommEvent(ex: ReplayException)                           extends Failed
final case class ReplayStatusMismatch(replay: DeployStatus, orig: DeployStatus) extends Failed

case object UnknownFailure                       extends Failed
final case class UserError(error: Throwable)     extends Failed
final case class InternalError(error: Throwable) extends Failed
//TODO add fatal error related to rspace closed after https://github.com/rchain/rchain/pull/1339 is merged

object DeployStatus {
  def fromErrors(errors: Option[Throwable]): DeployStatus =
    errors match {
      case Some(e: InterpreterError) => UserError(e)
      case Some(e: ReplayException)  => UnusedCommEvent(e)
      case Some(e)                   => InternalError(e)
      case None                      => Succeeded
    }
}
