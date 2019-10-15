package coop.rchain.casper.util.rholang

import coop.rchain.casper.protocol.DeployData
import coop.rchain.rspace.ReplayException

sealed trait ReplayFailure

object ReplayFailure {

  def internalError(deploy: DeployData, throwable: Throwable): ReplayFailure =
    InternalError(deploy, throwable)

  def replayStatusMismatch(initialFailed: Boolean, replayFailed: Boolean): ReplayFailure =
    ReplayStatusMismatch(initialFailed, replayFailed)

  def unusedCOMMEvent(replayException: ReplayException): ReplayFailure =
    UnusedCOMMEvent(replayException)

}

final case class InternalError(deploy: DeployData, throwable: Throwable) extends ReplayFailure
final case class ReplayStatusMismatch(initialFailed: Boolean, replayFailed: Boolean)
    extends ReplayFailure
final case class UnusedCOMMEvent(replayException: ReplayException) extends ReplayFailure
