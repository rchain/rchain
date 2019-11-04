package coop.rchain.casper.util.rholang

import coop.rchain.casper.protocol.ProcessedSystemDeploy
import coop.rchain.casper.util.EventConverter
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.SystemDeployUserError.SystemDeployError

sealed trait SystemDeployResult[+A] {
  val stateHashOpt: Option[StateHash]
  val resultOpt: Option[A]
}

sealed trait SystemDeployPlayResult[A] extends SystemDeployResult[A] {
  val processedSystemDeploy: ProcessedSystemDeploy
}

object SystemDeployPlayResult {

  import coop.rchain.rspace.trace.Log

  final case class PlaySucceeded[A](
      stateHash: StateHash,
      processedSystemDeploy: ProcessedSystemDeploy.Succeeded,
      result: A
  ) extends SystemDeployPlayResult[A] {
    val stateHashOpt: Option[StateHash] = Some(stateHash)
    val resultOpt: Option[A]            = Some(result)
  }

  def playSucceeded[A](stateHash: StateHash, log: Log, result: A): SystemDeployPlayResult[A] =
    PlaySucceeded(
      stateHash,
      ProcessedSystemDeploy.Succeeded(log.map(EventConverter.toCasperEvent).toList),
      result
    )

  final case class PlayFailed[A](processedSystemDeploy: ProcessedSystemDeploy.Failed)
      extends SystemDeployPlayResult[A] {
    val stateHashOpt: Option[StateHash] = None
    val resultOpt: Option[A]            = None
  }

  def playFailed[A](log: Log, systemDeployError: SystemDeployError): SystemDeployPlayResult[A] =
    PlayFailed(
      ProcessedSystemDeploy
        .Failed(log.map(EventConverter.toCasperEvent).toList, systemDeployError.errorMsg)
    )
}

sealed trait SystemDeployReplayResult[A] extends SystemDeployResult[A]

object SystemDeployReplayResult {

  final case class ReplaySucceeded[A](stateHash: StateHash, result: A)
      extends SystemDeployReplayResult[A] {
    val stateHashOpt: Option[StateHash] = Some(stateHash)
    val resultOpt: Option[A]            = Some(result)
  }

  def replaySucceeded[A](stateHash: StateHash, result: A): SystemDeployReplayResult[A] =
    ReplaySucceeded(stateHash, result)

  final case class ReplayFailed[A](systemDeployError: SystemDeployError)
      extends SystemDeployReplayResult[A] {
    val stateHashOpt: Option[StateHash] = None
    val resultOpt: Option[A]            = None
  }

  def replayFailed[A](systemDeployError: SystemDeployError): SystemDeployReplayResult[A] =
    ReplayFailed(systemDeployError)

}
