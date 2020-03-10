package coop.rchain.comm

import coop.rchain.grpc.{GrpcServer, Server}
import coop.rchain.metrics.Metrics

import com.google.protobuf.ByteString
import io.grpc.netty.NettyServerBuilder
import monix.eval.Task
import monix.execution.Scheduler

package object discovery {
  val DiscoveryMetricsSource: Metrics.Source =
    Metrics.Source(CommMetricsSource, "discovery.kademlia")

  def acquireKademliaRPCServer(
      networkId: String,
      port: Int,
      pingHandler: PeerNode => Task[Unit],
      ioScheduler: Scheduler,
      lookupHandler: (PeerNode, Array[Byte]) => Task[Seq[PeerNode]]
  )(implicit mainScheduler: Scheduler): Task[Server[Task]] =
    GrpcServer[Task](
      NettyServerBuilder
        .forPort(port)
        .executor(ioScheduler)
        .addService(
          KademliaGrpcMonix
            .bindService(
              new GrpcKademliaRPCServer(networkId, pingHandler, lookupHandler),
              mainScheduler
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
