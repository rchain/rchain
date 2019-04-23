package coop.rchain.casper.util.rholang

import coop.rchain.casper.protocol._
import coop.rchain.casper.util.EventConverter
import coop.rchain.models.PCost
import coop.rchain.rspace.trace

// TODO: Add post-state to individual InternalProcessedDeploy.
final case class InternalProcessedDeploy(
    deploy: DeployData,
    cost: PCost,
    deployLog: Seq[trace.Event],
    paymentLog: Seq[trace.Event],
    status: DeployStatus
)

object ProcessedDeployUtil {

  def toInternal(pd: ProcessedDeploy): Option[InternalProcessedDeploy] =
    for {
      d <- pd.deploy
      c <- pd.cost
      l = pd.log.map(EventConverter.toRspaceEvent)
      s = if (pd.errored) UnknownFailure else Succeeded
    } yield InternalProcessedDeploy(d, c, l, Seq.empty[trace.Event], s)

  def fromInternal(ipd: InternalProcessedDeploy): ProcessedDeploy = ipd match {
    case InternalProcessedDeploy(deploy, cost, deployLog, _, status) =>
      ProcessedDeploy(
        deploy = Some(deploy),
        cost = Some(cost),
        log = deployLog.map(EventConverter.toCasperEvent),
        errored = status.isFailed
      )
  }
}
