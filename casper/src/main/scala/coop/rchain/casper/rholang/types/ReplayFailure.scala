package coop.rchain.casper.rholang.types

import coop.rchain.rspace.util.ReplayException

sealed trait ReplayFailure

object ReplayFailure {

  def internalError(throwable: Throwable): ReplayFailure = InternalError(throwable)

  def replayStatusMismatch(initialFailed: Boolean, replayFailed: Boolean): ReplayFailure =
    ReplayStatusMismatch(initialFailed, replayFailed)

  def unusedCOMMEvent(replayException: ReplayException): ReplayFailure =
    UnusedCOMMEvent(replayException)

  def replayCostMismatch(initialCost: Long, replayCost: Long): ReplayFailure =
    ReplayCostMismatch(initialCost, replayCost)

  def systemDeployErrorMismatch(playError: String, replayError: String): ReplayFailure =
    SystemDeployErrorMismatch(playError, replayError)

}

final case class InternalError(throwable: Throwable) extends ReplayFailure
final case class ReplayStatusMismatch(initialFailed: Boolean, replayFailed: Boolean)
    extends ReplayFailure
final case class UnusedCOMMEvent(replayException: ReplayException)       extends ReplayFailure
final case class ReplayCostMismatch(initialCost: Long, replayCost: Long) extends ReplayFailure
final case class SystemDeployErrorMismatch(playError: String, replayError: String)
    extends ReplayFailure
