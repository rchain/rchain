package coop.rchain.casper.util.rholang

import coop.rchain.casper.util.EventConverter
import coop.rchain.casper.protocol._
import coop.rchain.models.PCost
import coop.rchain.rspace.trace

object ProcessedDeployUtil {
  type InternalProcessedDeploy = (Deploy, PCost, Seq[trace.Event], DeployStatus)

  def toInternal(pd: ProcessedDeploy): Option[InternalProcessedDeploy] =
    for {
      d <- pd.deploy
      c <- pd.cost
      l = pd.log.map(EventConverter.toRspaceEvent)
      s = if (pd.errored) UnknownFailure else Succeeded
    } yield (d, c, l, s)

  def fromInternal(ipd: InternalProcessedDeploy): ProcessedDeploy = ipd match {
    case (deploy, cost, log, status) =>
      ProcessedDeploy(
        deploy = Some(deploy),
        cost = Some(cost),
        log = log.map(EventConverter.toCasperEvent),
        errored = status.isFailed
      )
  }
}
