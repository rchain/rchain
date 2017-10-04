package coop.rchain.comm

import coop.rchain.kademlia.{Peer => KademliaPeer}

// Implementation of Peer for kademlia protocol.
class PeerNode(val pKey: Array[Byte]) extends KademliaPeer {
  val key = pKey

  override def ping =
    println(s"PING: $this")

  lazy private val sKey = key map { "%02x" format _ } mkString
  override def toString = s"#{PeerNode $sKey}"
}
