package coop.rchain.comm.rp

import scala.concurrent.duration._

case class RPConf(defaultTimeout: FiniteDuration, clearConnections: ClearConnetionsConf)
case class ClearConnetionsConf(maxNumOfConnections: Int, numOfConnectionsPinged: Int)
