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
final case class UserErrors(errors: Vector[Throwable])                          extends Failed
final case class InternalErrors(errors: Vector[Throwable])                      extends Failed
//TODO add fatal error related to rspace closed after https://github.com/rchain/rchain/pull/1339 is merged

object DeployStatus {
  def fromErrors(errors: Vector[Throwable]): DeployStatus = {
    val (userErrors, internalErrors) = errors.partition {
      case _: InterpreterError => true
      case _                   => false
    }

    internalErrors
      .collectFirst { case ex: ReplayException => ex }
      .fold[DeployStatus](
        if (internalErrors.nonEmpty) InternalErrors(internalErrors)
        else if (userErrors.nonEmpty) UserErrors(userErrors)
        else Succeeded
      )(ex => UnusedCommEvent(ex))
  }
}
