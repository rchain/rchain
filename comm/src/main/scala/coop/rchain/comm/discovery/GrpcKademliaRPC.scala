package coop.rchain.comm.discovery

import cats._, cats.data._, cats.implicits._, cats.mtl._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import coop.rchain.comm._
import monix.eval._
import monix.execution.atomic._
import monix.execution._
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Cell, Log, LogSource}
import coop.rchain.comm.protocol.routing._
import scala.concurrent.Future
import io.grpc._, io.grpc.netty._
import com.google.protobuf.ByteString

class GrpcKademliaRPC(port: Int)(implicit
                                 scheduler: Scheduler,
                                 metrics: Metrics[Task],
                                 log: Log[Task])
    extends KademliaRPC[Task] {

  private implicit val logSource: LogSource = LogSource(this.getClass)

  def ping(peer: PeerNode): Task[Boolean] =
    for {
      _       <- Metrics[Task].incrementCounter("protocol-ping-sends")
      channel <- clientChannel(peer)
      pongErr <- Task.delay {
                  KademliaRPCServiceGrpc.blockingStub(channel).sendPing(Ping())
                }.attempt
    } yield pongErr.fold(kp(false), kp(true))

  def lookup(key: Seq[Byte], peer: PeerNode): Task[Seq[PeerNode]] =
    for {
      _       <- Metrics[Task].incrementCounter("protocol-lookup-send")
      lookup  = Lookup().withId(ByteString.copyFrom(key.toArray))
      channel <- clientChannel(peer)
      responseErr <- Task.delay {
                      KademliaRPCServiceGrpc.blockingStub(channel).sendLookup(lookup)
                    }.attempt
    } yield
      responseErr.fold(
        kp(Seq.empty[PeerNode]),
        lr => lr.nodes.map(ProtocolHelper.toPeerNode)
      )

  def receive(pingHandler: Ping => Task[Pong],
              lookupHandler: Lookup => Task[LookupResponse]): Task[Unit] = Task.delay {
    NettyServerBuilder
      .forPort(port)
      .addService(KademliaRPCServiceGrpc
        .bindService(new SimpleKademliaRPCService[Task](pingHandler, lookupHandler), scheduler))
      .build
      .start
  }

  private def clientChannel(peer: PeerNode): Task[ManagedChannel] =
    for {
      _ <- log.debug(s"Creating new channel to peer ${peer.toAddress}")
      c <- Task.delay {
            NettyChannelBuilder.forAddress(peer.endpoint.host, peer.endpoint.udpPort).build()
          }
    } yield c

}

class SimpleKademliaRPCService[F[_]: Futurable](
    pingHandler: Ping => F[Pong],
    lookupHandler: Lookup => F[LookupResponse]
) extends KademliaRPCServiceGrpc.KademliaRPCService {
  def sendLookup(lookup: Lookup): Future[LookupResponse] = lookupHandler(lookup).toFuture
  def sendPing(ping: Ping): Future[Pong]                 = pingHandler(ping).toFuture
}
