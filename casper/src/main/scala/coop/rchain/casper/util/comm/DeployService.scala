package coop.rchain.casper.util.comm

import java.io.Closeable
import java.util.concurrent.TimeUnit

import scala.util.Either

import cats.implicits._

import coop.rchain.casper.protocol._
import coop.rchain.casper.protocol.deploy.v1.DeployServiceV1GrpcMonix
import coop.rchain.models.either.implicits._

import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.eval.Task

trait DeployService[F[_]] {
  def deploy(d: DeployData): F[Either[Seq[String], String]]
  def getBlock(q: BlockQuery): F[Either[Seq[String], String]]
  def getBlocks(q: BlocksQuery): F[Either[Seq[String], String]]
  def visualizeDag(q: VisualizeDagQuery): F[Either[Seq[String], String]]
  def machineVerifiableDag(q: MachineVerifyQuery): F[Either[Seq[String], String]]
  def findDeploy(request: FindDeployQuery): F[Either[Seq[String], String]]
  def listenForDataAtName(request: DataAtNameQuery): F[Either[Seq[String], Seq[DataWithBlockInfo]]]
  def listenForContinuationAtName(
      request: ContinuationAtNameQuery
  ): F[Either[Seq[String], Seq[ContinuationsWithBlockInfo]]]
  def lastFinalizedBlock: F[Either[Seq[String], String]]
  def isFinalized(q: IsFinalizedQuery): F[Either[Seq[String], String]]
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

  private val stub = DeployServiceV1GrpcMonix.stub(channel)

  def deploy(d: DeployData): Task[Either[Seq[String], String]] =
    stub
      .doDeploy(d.toProto)
      .toEitherF(
        _.message.error,
        _.message.result
      )

  def getBlock(q: BlockQuery): Task[Either[Seq[String], String]] =
    stub
      .getBlock(q)
      .toEitherF(
        _.message.error,
        _.message.blockInfo.map(_.toProtoString)
      )

  def findDeploy(q: FindDeployQuery): Task[Either[Seq[String], String]] =
    stub
      .findDeploy(q)
      .toEitherF(
        _.message.error,
        _.message.blockInfo.map(_.toProtoString)
      )

  def visualizeDag(q: VisualizeDagQuery): Task[Either[Seq[String], String]] =
    stub
      .visualizeDag(q)
      .mapEval(_.pure[Task].toEitherF(_.message.error, _.message.content))
      .toListL
      .map { bs =>
        val (l, r) = bs.partition(_.isLeft)
        if (l.isEmpty) Right(r.map(_.right.get).mkString)
        else Left(l.flatMap(_.left.get))
      }

  def machineVerifiableDag(q: MachineVerifyQuery): Task[Either[Seq[String], String]] =
    stub
      .machineVerifiableDag(q)
      .toEitherF(
        _.message.error,
        _.message.content
      )

  def getBlocks(q: BlocksQuery): Task[Either[Seq[String], String]] =
    stub
      .getBlocks(q)
      .mapEval(_.pure[Task].toEitherF(_.message.error, _.message.blockInfo))
      .map(_.map { bi =>
        s"""
         |------------- block ${bi.blockNumber} ---------------
         |${bi.toProtoString}
         |-----------------------------------------------------
         |""".stripMargin
      })
      .toListL
      .map { bs =>
        val (l, r) = bs.partition(_.isLeft)
        if (l.isEmpty) {
          val showLength =
            s"""
             |count: ${r.length}
             |""".stripMargin

          Right(r.map(_.right.get).mkString("\n") + "\n" + showLength)
        } else Left(l.flatMap(_.left.get))
      }

  def listenForDataAtName(
      request: DataAtNameQuery
  ): Task[Either[Seq[String], Seq[DataWithBlockInfo]]] =
    stub
      .listenForDataAtName(request)
      .toEitherF(
        _.message.error,
        _.message.payload.map(_.blockInfo)
      )

  def listenForContinuationAtName(
      request: ContinuationAtNameQuery
  ): Task[Either[Seq[String], Seq[ContinuationsWithBlockInfo]]] =
    stub
      .listenForContinuationAtName(request)
      .toEitherF(
        _.message.error,
        _.message.payload.map(_.blockResults)
      )

  def lastFinalizedBlock: Task[Either[Seq[String], String]] =
    stub
      .lastFinalizedBlock(LastFinalizedBlockQuery())
      .toEitherF(
        _.message.error,
        _.message.blockInfo.map(_.toProtoString)
      )

  def isFinalized(request: IsFinalizedQuery): Task[Either[Seq[String], String]] =
    stub
      .isFinalized(request)
      .toEitherF(_.message.error, _.message.isFinalized)
      .map(
        _.ifM(
          "Block is finalized".asRight,
          Seq("Block is not finalized").asLeft
        )
      )

  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
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
