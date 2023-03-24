package coop.rchain.comm.discovery

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.comm.PeerNode
import coop.rchain.monix.Monixable
import io.grpc.Metadata

class GrpcKademliaRPCServer[F[_]: Monixable: Sync](
    networkId: String,
    pingHandler: PeerNode => F[Unit],
    lookupHandler: (PeerNode, Array[Byte]) => F[Seq[PeerNode]]
) extends KademliaRPCServiceFs2Grpc[F, Metadata] {

  // TODO: legacy code generates KademliaGrpcMonix methods with Task
  //  so these methods cannot be abstracted over effect type

  override def sendLookup(lookup: Lookup, ctx: Metadata): F[LookupResponse] =
    if (lookup.networkId == networkId) {
      val id               = lookup.id.toByteArray
      val sender: PeerNode = toPeerNode(lookup.sender.get)
      lookupHandler(sender, id)
        .map(peers => LookupResponse().withNodes(peers.map(toNode)).withNetworkId(networkId))
    } else Sync[F].delay(LookupResponse().withNodes(Nil))

  override def sendPing(ping: Ping, ctx: Metadata): F[Pong] =
    if (ping.networkId == networkId) {
      val sender: PeerNode = toPeerNode(ping.sender.get)
      pingHandler(sender).as(Pong().withNetworkId(networkId))
    } else Sync[F].delay(Pong().withNetworkId(networkId))
}
