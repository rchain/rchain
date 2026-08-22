package coop.rchain.comm.discovery

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.comm.PeerNode
import coop.rchain.monix.Monixable
import coop.rchain.shared.syntax._
import monix.eval.Task

class GrpcKademliaRPCServer[F[_]: Monixable: Sync](
    networkId: String,
    pingHandler: PeerNode => F[Unit],
    lookupHandler: (PeerNode, Array[Byte]) => F[Seq[PeerNode]]
) extends KademliaGrpcMonix.KademliaRPCService {

  // TODO: legacy code generates KademliaGrpcMonix methods with Task
  //  so these methods cannot be abstracted over effect type

  def sendLookup(lookup: Lookup): Task[LookupResponse] =
    if (lookup.networkId == networkId) {
      val id               = lookup.id.toByteArray
      val sender: PeerNode = toPeerNode(lookup.sender.get)
      lookupHandler(sender, id)
        .map(peers => LookupResponse().withNodes(peers.map(toNode)).withNetworkId(networkId))
        .toTask
    } else Sync[F].delay(LookupResponse().withNodes(Nil)).toTask

  def sendPing(ping: Ping): Task[Pong] =
    if (ping.networkId == networkId) {
      val sender: PeerNode = toPeerNode(ping.sender.get)
      pingHandler(sender).toTask.as(Pong().withNetworkId(networkId))
    } else Sync[F].delay(Pong().withNetworkId(networkId)).toTask

}
