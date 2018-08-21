package coop.rchain.casper.util.comm

import java.io.Closeable
import java.util.concurrent.TimeUnit

import cats.implicits._

import com.google.protobuf.empty.Empty
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import coop.rchain.casper.protocol.{BlockMessage, BlockQuery, DeployData, DeployServiceGrpc}

import monix.eval.Task

trait DeployService[F[_]] {
  def deploy(d: DeployData): F[(Boolean, String)]
  def createBlock(): F[(Boolean, String)] //create block and add to Casper internal state
  def showBlock(q: BlockQuery): F[String]
  def showBlocks(): F[String]
  def addBlock(b: BlockMessage): F[(Boolean, String)]
}

object DeployService {
  def apply[F[_]](implicit ev: DeployService[F]): DeployService[F] = ev
}

class GrpcDeployService(host: String, port: Int) extends DeployService[Task] with Closeable {

  private val channel: ManagedChannel =
    ManagedChannelBuilder.forAddress(host, port).usePlaintext(true).build
  private val blockingStub = DeployServiceGrpc.blockingStub(channel)

  def deploy(d: DeployData): Task[(Boolean, String)] = Task.delay {
    val response = blockingStub.doDeploy(d)
    (response.success, response.message)
  }

  def createBlock(): Task[(Boolean, String)] =
    Task.delay {
      val response = blockingStub.createBlock(Empty())
      (response.success, response.message)
    }

  def showBlock(q: BlockQuery): Task[String] = Task.delay {
    val response = blockingStub.showBlock(q)
    response.toProtoString
  }

  def showBlocks(): Task[String] = Task.delay {
    val response = blockingStub.showBlocks(Empty()).toList

    val showResponses = response
      .map {
        case bi =>
          s"""
------------- block ${bi.blockNumber} ---------------
${bi.toProtoString}
-----------------------------------------------------
"""
      }
      .mkString("\n")

    val showLength =
      s"""
Blockchain length: ${response.length}
"""
    showResponses + "\n" + showLength
  }

  def addBlock(b: BlockMessage): Task[(Boolean, String)] = Task.delay {
    val response = blockingStub.addBlock(b)
    (response.success, response.message)
  }

  override def close(): Unit = {
    val terminated = channel.shutdown().awaitTermination(10, TimeUnit.SECONDS)
    if (!terminated) {
      println(
        "warn: did not shutdown after 10 seconds, retrying with additional 10 seconds timeout")
      channel.awaitTermination(10, TimeUnit.SECONDS)
    }
  }
}
