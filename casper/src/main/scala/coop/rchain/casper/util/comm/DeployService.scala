package coop.rchain.casper.util.comm

import java.io.Closeable
import java.util.concurrent.TimeUnit

import scala.util.Either

import coop.rchain.casper.protocol._

import com.google.protobuf.empty.Empty
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.eval.Task

trait DeployService[F[_]] {
  def deploy(d: DeployData): F[Either[Throwable, String]]
  def createBlock(): F[Either[Throwable, String]] //create block and add to Casper internal state
  def showBlock(q: BlockQuery): F[Either[Throwable, String]]
  def showBlocks(q: BlocksQuery): F[Either[Throwable, String]]
  def addBlock(b: BlockMessage): F[Either[Throwable, String]]
  def listenForDataAtName(request: DataAtNameQuery): F[ListeningNameDataResponse]
  def listenForContinuationAtName(
      request: ContinuationAtNameQuery
  ): F[ListeningNameContinuationResponse]
}

object DeployService {
  def apply[F[_]](implicit ev: DeployService[F]): DeployService[F] = ev
}

class GrpcDeployService(host: String, port: Int, maxMessageSize: Int)
    extends DeployService[Task]
    with Closeable {

  private val channel: ManagedChannel =
    ManagedChannelBuilder
      .forAddress(host, port)
      .maxInboundMessageSize(maxMessageSize)
      .usePlaintext()
      .build

  private val stub = CasperMessageGrpcMonix.stub(channel)

  def deploy(d: DeployData): Task[Either[Throwable, String]] =
    stub.doDeploy(d).map { response =>
      if (response.success) Right(response.message)
      else Left(new RuntimeException(response.message))
    }

  def createBlock(): Task[Either[Throwable, String]] =
    stub.createBlock(Empty()).map { response =>
      if (response.success) Right(response.message)
      else Left(new RuntimeException(response.message))
    }

  def showBlock(q: BlockQuery): Task[Either[Throwable, String]] =
    stub.showBlock(q).map { response =>
      if (response.status == "Success") Right(response.toProtoString)
      else Left(new RuntimeException(response.status))
    }

  def showBlocks(q: BlocksQuery): Task[Either[Throwable, String]] =
    stub
      .showBlocks(q)
      .map { bi =>
        s"""
         |------------- block ${bi.blockNumber} ---------------
         |${bi.toProtoString}
         |-----------------------------------------------------
         |""".stripMargin
      }
      .toListL
      .map { bs =>
        val showLength =
          s"""
           |count: ${bs.length}
           |""".stripMargin

        Right(bs.mkString("\n") + "\n" + showLength)
      }

  def addBlock(b: BlockMessage): Task[Either[Throwable, String]] =
    stub.addBlock(b).map { response =>
      if (response.success) Right(response.message)
      else Left(new RuntimeException(response.message))
    }

  def listenForDataAtName(request: DataAtNameQuery): Task[ListeningNameDataResponse] =
    stub.listenForDataAtName(request)

  def listenForContinuationAtName(
      request: ContinuationAtNameQuery
  ): Task[ListeningNameContinuationResponse] =
    stub.listenForContinuationAtName(request)

  override def close(): Unit = {
    val terminated = channel.shutdown().awaitTermination(10, TimeUnit.SECONDS)
    if (!terminated) {
      println(
        "warn: did not shutdown after 10 seconds, retrying with additional 10 seconds timeout"
      )
      channel.awaitTermination(10, TimeUnit.SECONDS)
    }
  }
}
