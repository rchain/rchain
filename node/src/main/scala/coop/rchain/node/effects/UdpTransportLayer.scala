package coop.rchain.node.effects

import scala.concurrent.duration._
import coop.rchain.comm._, CommError._
import coop.rchain.comm.protocol.routing._
import coop.rchain.p2p.effects._
import coop.rchain.metrics.Metrics
import io.grpc.{ManagedChannel, ManagedChannelBuilder, Server, ServerBuilder}

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import monix.eval.Task

class UdpTransportLayer(src: PeerNode)(implicit
                                       ev1: Log[Task],
                                       ev2: Time[Task],
                                       ev3: Metrics[Task])
    extends TransportLayer[Task] {

  val net = new UnicastNetwork(src)

  def roundTrip(msg: ProtocolMessage,
                remote: ProtocolNode,
                timeout: Duration): Task[CommErr[ProtocolMessage]] =
    net.roundTrip[Task](msg, remote, timeout)

  def local: Task[ProtocolNode] = net.local.pure[Task]

  def send(msg: ProtocolMessage, peer: PeerNode): Task[CommErr[Unit]] =
    Task.delay(net.comm.send(msg.toByteSeq, peer))

  def broadcast(msg: ProtocolMessage, peers: Seq[PeerNode]): Task[Seq[CommErr[Unit]]] =
    peers.toList.traverse(peer => send(msg, peer)).map(_.toSeq)

  private def handle(dispatch: ProtocolMessage => Task[CommunicationResponse])
    : Option[ProtocolMessage] => Task[Unit] = _.fold(().pure[Task]) { pm =>
    for {
      ti <- Time[Task].nanoTime
      r1 <- dispatch(pm)
      r2 <- r1 match {
             case NotHandled | HandledWitoutMessage => ().pure[Task]
             case HandledWithMessage(response) => {

               pm.sender.fold(Log[Task].error(s"Sender not available for $pm")) { sender =>
                 send(response, sender) >>= {
                   case Left(error) =>
                     Log[Task].warn(
                       s"Was unable to send response $response for request: $pm, error: $error")
                   case _ => ().pure[Task]
                 }
               }
             }
           }
      tf <- Time[Task].nanoTime
      _  <- Metrics[Task].record("network-roundtrip-micros", (tf - ti) / 1000)
    } yield r2
  }

  def receive(dispatch: ProtocolMessage => Task[CommunicationResponse]): Task[Unit] =
    net
      .receiver[Task]
      .flatMap(handle(dispatch))
      .forever
      .executeAsync
      .start
      .void
}
