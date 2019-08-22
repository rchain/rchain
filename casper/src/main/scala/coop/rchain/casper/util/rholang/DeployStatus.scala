package coop.rchain.casper.util.rholang

import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.rspace.ReplayException

sealed trait DeployStatus { self =>
  def isFailed: Boolean = self match {
    case _: Failed => true
    case _         => false
  }

  def isInternalError = self match {
    case _: InternalErrors => true
    case _                 => false
  }
}

final case object Succeeded                                                     extends DeployStatus
sealed trait Failed                                                             extends DeployStatus
final case class UnusedCommEvent(ex: ReplayException)                           extends Failed
final case class ReplayStatusMismatch(replay: DeployStatus, orig: DeployStatus) extends Failed
final case object UnknownFailure                                                extends Failed
final case class UserErrors(errors: Seq[Throwable])                             extends Failed
final case class InternalErrors(errors: Seq[Throwable])                         extends Failed
//TODO add fatal error related to rspace closed after https://github.com/rchain/rchain/pull/1339 is merged

// TODO: Make UserErrors and InternalErrors take Throwable
object DeployStatus {
  def fromErrors(errors: Option[Throwable]): DeployStatus =
    errors match {
      case Some(e: InterpreterError) => UserErrors(Seq(e))
      case Some(e: ReplayException)  => UnusedCommEvent(e)
      case Some(e)                   => InternalErrors(Seq(e))
      case None                      => Succeeded
    }
}
