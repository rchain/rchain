package coop.rchain.casper.util.rholang

import coop.rchain.casper.protocol._
import coop.rchain.casper.util.EventConverter
import coop.rchain.models.PCost
import coop.rchain.rspace.trace

final case class InternalProcessedDeploy(
    deploy: DeployData,
    cost: PCost,
    deployLog: Seq[trace.Event],
    isFailed: Boolean
) {

  def toProcessedDeploy: ProcessedDeploy =
    ProcessedDeploy(
      deploy = deploy,
      cost = cost,
      deployLog = deployLog.map(EventConverter.toCasperEvent).toList,
      errored = isFailed
    )

}

object InternalProcessedDeploy {

  def fromProcessedDeploy(pd: ProcessedDeploy): InternalProcessedDeploy =
    InternalProcessedDeploy(
      pd.deploy,
      pd.cost,
      pd.deployLog.map(EventConverter.toRspaceEvent),
      pd.errored
    )
}
