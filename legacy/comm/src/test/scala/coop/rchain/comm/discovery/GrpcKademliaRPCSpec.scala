package coop.rchain.comm.discovery

import cats.Applicative
import cats.effect.Resource
import cats.mtl.DefaultApplicativeAsk
import coop.rchain.comm._
import coop.rchain.comm.rp.Connect.RPConfAsk
import coop.rchain.comm.rp.RPConf
import coop.rchain.metrics.Metrics
import coop.rchain.shared.Log
import io.grpc
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._
import scala.util.Random

class GrpcKademliaRPCSpec extends KademliaRPCSpec[Task, GrpcEnvironment] {

  implicit val log: Log[Task]         = new Log.NOPLog[Task]
  implicit val scheduler: Scheduler   = Scheduler.Implicits.global
  implicit val metrics: Metrics[Task] = new Metrics.MetricsNOP
  private val networkId               = "test"

  def createEnvironment(port: Int): Task[GrpcEnvironment] =
    Task.delay {
      val host  = "127.0.0.1"
      val bytes = Array.ofDim[Byte](40)
      Random.nextBytes(bytes)
      val peer = PeerNode.from(NodeIdentifier(bytes), host, 0, port)
      GrpcEnvironment(host, port, peer)
    }

  def createKademliaRPC(env: GrpcEnvironment): Task[KademliaRPC[Task]] = {
    implicit val ask: RPConfAsk[Task] =
      new DefaultApplicativeAsk[Task, RPConf] {
        val applicative: Applicative[Task] = Applicative[Task]
        def ask: Task[RPConf] = Task.pure(
          RPConf(local = env.peer, null, null, null, 0, null)
        )
      }
    Task.delay(new GrpcKademliaRPC(networkId, 500.millis))
  }

  def extract[A](fa: Task[A]): A = fa.runSyncUnsafe(Duration.Inf)

  def createKademliaRPCServer(
      env: GrpcEnvironment,
      pingHandler: PeerNode => Task[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => Task[Seq[PeerNode]]
  ): Resource[Task, grpc.Server] =
    acquireKademliaRPCServer(networkId, env.port, pingHandler, lookupHandler, scheduler)
}

case class GrpcEnvironment(
    host: String,
    port: Int,
    peer: PeerNode
) extends Environment
