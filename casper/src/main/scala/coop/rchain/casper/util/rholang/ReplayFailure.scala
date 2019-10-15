package coop.rchain.casper.util.rholang

import coop.rchain.casper.protocol.DeployData
import coop.rchain.rspace.ReplayException

sealed trait ReplayFailure

object ReplayFailure {

  def internalError(deploy: DeployData, throwable: Throwable): ReplayFailure =
    InternalError(deploy, throwable)

  def replayStatusMismatch(initialFailed: Boolean, replayFailed: Boolean): ReplayFailure =
    ReplayStatusMismatch(initialFailed, replayFailed)

  def unusedCOMMEvent(deployData: DeployData, replayException: ReplayException): ReplayFailure =
    UnusedCOMMEvent(deployData, replayException)

  def replayCostMismatch(
      deployData: DeployData,
      initialCost: Long,
      replayCost: Long
  ): ReplayFailure =
    ReplayCostMismatch(deployData, initialCost, replayCost)

}

final case class InternalError(deploy: DeployData, throwable: Throwable) extends ReplayFailure
final case class ReplayStatusMismatch(initialFailed: Boolean, replayFailed: Boolean)
    extends ReplayFailure
final case class UnusedCOMMEvent(deployData: DeployData, replayException: ReplayException)
    extends ReplayFailure
final case class ReplayCostMismatch(deployData: DeployData, initialCost: Long, replayCost: Long)
    extends ReplayFailure
