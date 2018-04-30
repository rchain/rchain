package coop.rchain.node

import java.util.concurrent.TimeUnit
import io.grpc.{ManagedChannel, ManagedChannelBuilder, StatusRuntimeException}
import coop.rchain.node.rnode._
import monix.eval.Task

trait DiagnosticsService[F[_]] {
  def listPeers: F[List[Peer]]
}

object DiagnosticsService {
  def apply[F[_]](implicit ev: DiagnosticsService[F]): DiagnosticsService[F] = ev
}

class GrpcDiagnosticsService(host: String, port: Int) extends DiagnosticsService[Task] {

  private val channel: ManagedChannel =
    ManagedChannelBuilder.forAddress(host, port).usePlaintext(true).build
  private val blockingStub = DiagnosticsGrpc.blockingStub(channel)

  def listPeers: Task[List[Peer]] = Task.delay {
    blockingStub.listPeers(ListPeersRequest()).peer.toList
  }
}
