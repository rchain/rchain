package coop.rchain.comm.discovery

import cats.implicits._

import coop.rchain.comm.PeerNode

import monix.eval.Task

class GrpcKademliaRPCServer(
    networkId: String,
    pingHandler: PeerNode => Task[Unit],
    lookupHandler: (PeerNode, Array[Byte]) => Task[Seq[PeerNode]]
) extends KademliaGrpcMonix.KademliaRPCService {

  def sendLookup(lookup: Lookup): Task[LookupResponse] =
    if (lookup.networkId == networkId) {
      val id               = lookup.id.toByteArray
      val sender: PeerNode = toPeerNode(lookup.sender.get)
      lookupHandler(sender, id)
        .map(peers => LookupResponse().withNodes(peers.map(toNode)).withNetworkId(networkId))
    } else Task.now(LookupResponse().withNodes(Nil))

  def sendPing(ping: Ping): Task[Pong] =
    if (ping.networkId == networkId) {
      val sender: PeerNode = toPeerNode(ping.sender.get)
      pingHandler(sender).as(Pong().withNetworkId(networkId))
    } else Task.now(Pong().withNetworkId(networkId))

}
