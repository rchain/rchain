package coop.rchain.comm.rp

import scala.concurrent.duration._

import coop.rchain.comm.LocalPeerNode

case class RPConf(
    local: LocalPeerNode,
    defaultTimeout: FiniteDuration,
    clearConnections: ClearConnetionsConf
)
case class ClearConnetionsConf(maxNumOfConnections: Int, numOfConnectionsPinged: Int)
