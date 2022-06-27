package coop.rchain.comm.discovery

import scala.concurrent.duration._
import scala.util.Random

import cats.mtl.DefaultApplicativeAsk
import cats.Applicative

import coop.rchain.comm._
import coop.rchain.grpc.Server
import coop.rchain.metrics.Metrics
import coop.rchain.shared.Log

import monix.eval.Task
import monix.execution.Scheduler

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
    implicit val ask: PeerNodeAsk[Task] =
      new DefaultApplicativeAsk[Task, PeerNode] {
        val applicative: Applicative[Task] = Applicative[Task]
        def ask: Task[PeerNode]            = Task.pure(env.peer)
      }
    Task.delay {
      new GrpcKademliaRPC(networkId, 500.millis)
    }
  }

  def extract[A](fa: Task[A]): A = fa.runSyncUnsafe(Duration.Inf)

  def createKademliaRPCServer(
      env: GrpcEnvironment,
      pingHandler: PeerNode => Task[Unit],
      lookupHandler: (PeerNode, Array[Byte]) => Task[Seq[PeerNode]]
  ): Task[Server[Task]] = acquireKademliaRPCServer(networkId, env.port, pingHandler, lookupHandler)
}

case class GrpcEnvironment(
    host: String,
    port: Int,
    peer: PeerNode
) extends Environment
