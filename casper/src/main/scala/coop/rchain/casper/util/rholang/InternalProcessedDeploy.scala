package coop.rchain.casper.util.rholang

import coop.rchain.casper.protocol._
import coop.rchain.casper.util.EventConverter
import coop.rchain.models.PCost
import coop.rchain.rspace.trace

final case class InternalProcessedDeploy(
    deploy: DeployData,
    cost: PCost,
    deployLog: Seq[trace.Event],
    paymentLog: Seq[trace.Event],
    status: DeployStatus
) {

  def toProcessedDeploy: ProcessedDeploy =
    ProcessedDeploy(
      deploy = deploy,
      cost = cost,
      deployLog = deployLog.map(EventConverter.toCasperEvent).toList,
      paymentLog = paymentLog.map(EventConverter.toCasperEvent).toList,
      errored = status.isFailed
    )

}

object InternalProcessedDeploy {

  def fromProcessedDeploy(pd: ProcessedDeploy): InternalProcessedDeploy =
    InternalProcessedDeploy(
      pd.deploy,
      pd.cost,
      pd.deployLog.map(EventConverter.toRspaceEvent),
      pd.paymentLog.map(EventConverter.toRspaceEvent),
      if (pd.errored) UnknownFailure else Succeeded
    )
}
