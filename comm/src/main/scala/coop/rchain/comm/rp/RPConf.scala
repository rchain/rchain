package coop.rchain.comm.rp

import coop.rchain.comm.PeerNode
import scala.concurrent.duration._

case class RPConf(local: PeerNode,
                  defaultTimeout: FiniteDuration,
                  clearConnections: ClearConnetionsConf)
case class ClearConnetionsConf(maxNumOfConnections: Int, numOfConnectionsPinged: Int)
