package coop.rchain.casper.util.comm

import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import coop.rchain.casper.protocol.{Deploy, DeployServiceGrpc}
import monix.eval.Task

trait DeployService[F[_]] {
  def deploy(d: Deploy): F[Boolean]
}

object DeployService {
  def apply[F[_]](implicit ev: DeployService[F]): DeployService[F] = ev
}

class GrpcDeployService(host: String, port: Int) extends DeployService[Task] {

  private val channel: ManagedChannel =
    ManagedChannelBuilder.forAddress(host, port).usePlaintext(true).build
  private val blockingStub = DeployServiceGrpc.blockingStub(channel)

  def deploy(d: Deploy): Task[Boolean] = Task.delay {
    val response = blockingStub.doDeploy(d)
    response.success
  }
}
