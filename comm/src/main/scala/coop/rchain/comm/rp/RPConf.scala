package coop.rchain.comm.rp

import scala.concurrent.duration._

import coop.rchain.comm.PeerNode

case class RPConf(
    local: PeerNode,
    defaultTimeout: FiniteDuration,
    clearConnections: ClearConnetionsConf
)
case class ClearConnetionsConf(maxNumOfConnections: Int, numOfConnectionsPinged: Int)
