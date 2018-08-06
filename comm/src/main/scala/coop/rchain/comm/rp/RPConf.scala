package coop.rchain.comm.rp

case class RPConf(clearConnections: ClearConnetionsConf)
case class ClearConnetionsConf(maxNumOfConnections: Int, numOfConnectionsPinged: Int)
