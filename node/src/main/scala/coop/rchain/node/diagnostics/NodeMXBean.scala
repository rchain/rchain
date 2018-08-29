package coop.rchain.node.diagnostics

trait NodeMXBean {

  def getPingReceiverCount: Long
  def getLookupReceiverCount: Long
  def getDisconnectReceiverCount: Long
  def getConnects: Long
  def getP2pEncryptionHandshakeReceiverCount: Long
  def getP2pProtocolHandshakeReceiverCount: Long
  def getPeers: Long
  def getFrom: Long
  def getTo: Long
}

object NodeMXBean {
  val Name: String = "coop.rchain:type=Node"

  val PingReceiverCount                   = "PingReceiverCount"
  val LookupReceiverCount                 = "LookupReceiverCount"
  val DisconnectReceiverCount             = "DisconnectReceiverCount"
  val Connects                            = "Connects"
  val P2pEncryptionHandshakeReceiverCount = "P2pEncryptionHandshakeReceiverCount"
  val P2pProtocolHandshakeReceiverCount   = "P2pProtocolHandshakeReceiverCount"
  val Peers                               = "Peers"
  val From                                = "From"
  val To                                  = "To"

  val Attributes: Array[String] = Array(
    PingReceiverCount,
    LookupReceiverCount,
    DisconnectReceiverCount,
    Connects,
    P2pEncryptionHandshakeReceiverCount,
    P2pProtocolHandshakeReceiverCount,
    Peers,
    From,
    To
  )
}
