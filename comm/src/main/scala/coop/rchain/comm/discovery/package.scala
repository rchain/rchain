package coop.rchain.comm

import coop.rchain.grpc.{GrpcServer, Server}

import com.google.protobuf.ByteString
import io.grpc.netty.NettyServerBuilder
import monix.eval.Task
import monix.execution.Scheduler

package object discovery {
  def acquireKademliaRPCServer(
      port: Int,
      pingHandler: PeerNode => Task[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => Task[Seq[PeerNode]]
  )(implicit scheduler: Scheduler): Task[Server[Task]] =
    GrpcServer[Task](
      NettyServerBuilder
        .forPort(port)
        .executor(scheduler)
        .addService(
          KademliaGrpcMonix
            .bindService(new GrpcKademliaRPCServer(pingHandler, lookupHandler), scheduler)
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
