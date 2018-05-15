package coop.rchain.casper.util.comm

import cats.implicits._

import com.google.protobuf.empty.Empty

import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import coop.rchain.casper.protocol.{BlockQuery, DeployServiceGrpc, DeployString}
import monix.eval.Task

trait DeployService[F[_]] {
  def deploy(d: DeployString): F[(Boolean, String)]
  def propose(): F[Unit] //force Casper to propose a block
  def showBlock(q: BlockQuery): F[String]
}

object DeployService {
  def apply[F[_]](implicit ev: DeployService[F]): DeployService[F] = ev
}

class GrpcDeployService(host: String, port: Int) extends DeployService[Task] {

  private val channel: ManagedChannel =
    ManagedChannelBuilder.forAddress(host, port).usePlaintext(true).build
  private val blockingStub = DeployServiceGrpc.blockingStub(channel)

  def deploy(d: DeployString): Task[(Boolean, String)] = Task.delay {
    val response = blockingStub.doDeploy(d)
    (response.success, response.message)
  }

  def propose(): Task[Unit] =
    Task.delay {
      blockingStub.propose(Empty())
    }.void

  def showBlock(q: BlockQuery): Task[String] = Task.delay {
    val response = blockingStub.showBlock(q)
    response.desc
  }
}
