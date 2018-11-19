package coop.rchain.comm.rp

import scala.concurrent.duration._

import coop.rchain.comm.PeerNode

case class RPConf(
    local: PeerNode,
    dynamicLocal: PeerNode, // in case of a dynamic IP change
    bootstrap: Option[PeerNode],
    defaultTimeout: FiniteDuration,
    clearConnections: ClearConnetionsConf
)
case class ClearConnetionsConf(maxNumOfConnections: Int, numOfConnectionsPinged: Int)
