package coop.rchain.comm

import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.grpc.{GrpcServer, Server}
import coop.rchain.metrics.Metrics
import coop.rchain.monix.Monixable
import io.grpc.netty.NettyServerBuilder
import monix.execution.Scheduler

package object discovery {
  val DiscoveryMetricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "discovery.kademlia")

  def acquireKademliaRPCServer[F[_]: Monixable: Sync](
      networkId: String,
      port: Int,
      pingHandler: PeerNode => F[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => F[Seq[PeerNode]]
  )(implicit scheduler: Scheduler): F[Server[F]] =
    GrpcServer[F](
      NettyServerBuilder
        .forPort(port)
        .executor(scheduler)
        .addService(
          KademliaGrpcMonix
            .bindService(
              new GrpcKademliaRPCServer(networkId, pingHandler, lookupHandler),
              scheduler
            )
        )
        .build
    )

  def toPeerNode(n: Node): PeerNode =
    PeerNode(NodeIdentifier(n.id.toByteArray), Endpoint(n.host.toStringUtf8, n.tcpPort, n.udpPort))

  def toNode(n: PeerNode): Node =
    Node()
      .withId(ByteString.copyFrom(n.key.toArray))
      .withHost(ByteString.copyFromUtf8(n.endpoint.host))
      .withUdpPort(n.endpoint.udpPort)
      .withTcpPort(n.endpoint.tcpPort)
}
