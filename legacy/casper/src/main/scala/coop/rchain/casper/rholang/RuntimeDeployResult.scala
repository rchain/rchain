package coop.rchain.casper.rholang
import coop.rchain.casper.protocol.{ProcessedDeploy, ProcessedSystemDeploy}
import coop.rchain.rholang.interpreter.EvaluateResult
import coop.rchain.rspace.merger.EventLogMergingLogic.NumberChannelsEndVal

object RuntimeDeployResult {
  final case class UserDeployRuntimeResult(
      deploy: ProcessedDeploy,
      mergeable: NumberChannelsEndVal,
      evalResult: EvaluateResult
  )
  final case class SystemDeployRuntimeResult(
      deploy: ProcessedSystemDeploy,
      mergeable: NumberChannelsEndVal
  )
}
