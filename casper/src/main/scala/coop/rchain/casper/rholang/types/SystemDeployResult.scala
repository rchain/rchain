package coop.rchain.casper.rholang.types

import coop.rchain.casper.protocol.{Event, ProcessedSystemDeploy, SystemDeployData}
import coop.rchain.casper.rholang.RuntimeManager.StateHash
import coop.rchain.rspace.merger.MergingLogic.NumberChannelsEndVal

sealed trait SystemDeployResult[A]

final case class PlaySucceeded[A](
    stateHash: StateHash,
    processedSystemDeploy: ProcessedSystemDeploy.Succeeded,
    mergeableChannels: NumberChannelsEndVal,
    result: A
) extends SystemDeployResult[A]

final case class PlayFailed[A](processedSystemDeploy: ProcessedSystemDeploy.Failed)
    extends SystemDeployResult[A]

object SystemDeployResult {

  def playSucceeded[A](
      stateHash: StateHash,
      log: Seq[Event],
      systemDeployData: SystemDeployData,
      mergeableChannels: NumberChannelsEndVal,
      result: A
  ): SystemDeployResult[A] =
    PlaySucceeded(
      stateHash,
      ProcessedSystemDeploy.Succeeded(log.toList, systemDeployData),
      mergeableChannels,
      result
    )

  def playFailed[A](
      log: Seq[Event],
      systemDeployError: SystemDeployUserError
  ): SystemDeployResult[A] =
    PlayFailed(ProcessedSystemDeploy.Failed(log.toList, systemDeployError.errorMessage))

}
