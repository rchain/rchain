package coop.rchain.comm.rp

import scala.concurrent.duration._

import coop.rchain.comm.PeerNode

final case class RPConf(
    local: PeerNode,
    networkId: String,
    bootstrap: Option[PeerNode],
    maxNumOfConnections: Int,
    clearConnections: ClearConnectionsConf
)

final case class ClearConnectionsConf(numOfConnectionsPinged: Int)
