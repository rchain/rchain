package coop.rchain.casper.util.comm

import java.io.Closeable
import java.util.concurrent.TimeUnit

import scala.util.Either

import coop.rchain.casper.protocol._
import coop.rchain.models.either.implicits._

import com.google.protobuf.empty.Empty
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.eval.Task

trait DeployService[F[_]] {
  def deploy(d: DeployData): F[Either[Seq[String], String]]
  def createBlock(): F[Either[Seq[String], String]] //create block and add to Casper internal state
  def showBlock(q: BlockQuery): F[Either[Seq[String], String]]
  def showBlocks(q: BlocksQuery): F[Either[Seq[String], String]]
  def visualizeDag(q: VisualizeDagQuery): F[Either[Seq[String], String]]
  def listenForDataAtName(request: DataAtNameQuery): F[Either[Seq[String], Seq[DataWithBlockInfo]]]
  def listenForContinuationAtName(
      request: ContinuationAtNameQuery
  ): F[Either[Seq[String], Seq[ContinuationsWithBlockInfo]]]
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

  def deploy(d: DeployData): Task[Either[Seq[String], String]] =
    stub.doDeploy(d).map(_.toEither[DeployServiceResponse].map(_.message))

  def createBlock(): Task[Either[Seq[String], String]] =
    stub.createBlock(Empty()).map(_.toEither[DeployServiceResponse].map(_.message))

  def showBlock(q: BlockQuery): Task[Either[Seq[String], String]] =
    stub.showBlock(q).map(_.toEither[BlockQueryResponse].map(_.toProtoString))

  def visualizeDag(q: VisualizeDagQuery): Task[Either[Seq[String], String]] =
    stub.visualizeDag(q).map(_.toEither[VisualizeBlocksResponse].map(_.content))

  def showBlocks(q: BlocksQuery): Task[Either[Seq[String], String]] =
    stub
      .showBlocks(q)
      .map(_.toEither[BlockInfoWithoutTuplespace].map { bi =>
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
      .map(_.toEither[ListeningNameDataResponse].map(_.blockResults))

  def listenForContinuationAtName(
      request: ContinuationAtNameQuery
  ): Task[Either[Seq[String], Seq[ContinuationsWithBlockInfo]]] =
    stub
      .listenForContinuationAtName(request)
      .map(_.toEither[ListeningNameContinuationResponse].map(_.blockResults))

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
