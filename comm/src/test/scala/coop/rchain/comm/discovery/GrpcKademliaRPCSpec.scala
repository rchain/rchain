package coop.rchain.comm.discovery

import cats.Applicative
import cats.effect.{ContextShift, IO, Resource, Sync, Timer}
import cats.mtl.DefaultApplicativeAsk
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp.RPConf
import coop.rchain.metrics.Metrics
import coop.rchain.shared.{Log, RChainScheduler}
import io.grpc
import coop.rchain.shared.RChainScheduler._

import scala.concurrent.duration._
import scala.util.Random

class GrpcKademliaRPCSpec extends KademliaRPCSpec[IO, GrpcEnvironment] {

  implicit val log: Log[IO]         = new Log.NOPLog[IO]
  implicit val metrics: Metrics[IO] = new Metrics.MetricsNOP
  private val networkId             = "test"

  def createEnvironment(port: Int): IO[GrpcEnvironment] =
    IO.delay {
      val host  = "127.0.0.1"
      val bytes = Array.ofDim[Byte](40)
      Random.nextBytes(bytes)
      val peer = PeerNode.from(NodeIdentifier(bytes), host, 0, port)
      GrpcEnvironment(host, port, peer)
    }

  def createKademliaRPC(env: GrpcEnvironment): IO[KademliaRPC[IO]] = {
    implicit val ask: RPConfAsk[IO] =
      new DefaultApplicativeAsk[IO, RPConf] {
        val applicative: Applicative[IO] = Applicative[IO]
        def ask: IO[RPConf] = IO.pure(
          RPConf(local = env.peer, null, null, null, 0, null)
        )
      }
    IO.delay(new GrpcKademliaRPC(networkId, 500.millis, RChainScheduler.mainEC))
  }

  def extract[A](fa: IO[A]): A = fa.unsafeRunSync

  def createKademliaRPCServer(
      env: GrpcEnvironment,
      pingHandler: PeerNode => IO[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => IO[Seq[PeerNode]]
  ): Resource[IO, grpc.Server] =
    acquireKademliaRPCServer(
      networkId,
      env.port,
      pingHandler,
      lookupHandler,
      RChainScheduler.mainEC
    )
}

case class GrpcEnvironment(
    host: String,
    port: Int,
    peer: PeerNode
) extends Environment
