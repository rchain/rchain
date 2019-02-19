package coop.rchain.node.api

import cats.effect.concurrent.Semaphore
import cats.effect.Concurrent
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.protocol.{DeployData, DeployServiceResponse, VisualizeBlocksResponse, _}
import coop.rchain.shared._
import coop.rchain.graphz._
import coop.rchain.casper.api.{GraphConfig, GraphzGenerator}
import com.google.protobuf.empty.Empty
import cats._
import cats.data._
import cats.implicits._
import cats.mtl._
import cats.mtl.implicits._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.Taskable
import coop.rchain.catscontrib.TaskContrib._
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

private[api] object DeployGrpcService {
  def instance[F[_]: Concurrent: MultiParentCasperRef: Log: SafetyOracle: BlockStore: Taskable](
      blockApiLock: Semaphore[F]
  )(
      implicit worker: Scheduler
  ): CasperMessageGrpcMonix.DeployService =
    new CasperMessageGrpcMonix.DeployService {
      import ToErrorResponseInstances._
      private def defer[A: ToErrorResponse](task: F[A]): Task[A] =
        Task
          .defer(task.toTask)
          .executeOn(worker)
          .attemptAndLog
          .onErrorHandle(t => {
            ToErrorResponse[A].fromThrowable(t)
          })

      override def doDeploy(d: DeployData): Task[DeployServiceResponse] =
        defer(BlockAPI.deploy[F](d))

      override def createBlock(e: Empty): Task[DeployServiceResponse] =
        defer(BlockAPI.createBlock[F](blockApiLock))

      override def showBlock(q: BlockQuery): Task[BlockQueryResponse] =
        defer(BlockAPI.showBlock[F](q))

      // TODO handle potentiall errors (at least by returning proper response)
      override def visualizeDag(q: VisualizeDagQuery): Task[VisualizeBlocksResponse] = {
        type Effect[A] = StateT[Id, StringBuffer, A]
        implicit val ser: GraphSerializer[Effect]       = new StringSerializer[Effect]
        val stringify: Effect[Graphz[Effect]] => String = _.runS(new StringBuffer).toString

        val depth  = if (q.depth <= 0) None else Some(q.depth)
        val config = GraphConfig(q.showJustificationLines)

        defer(
          BlockAPI
            .visualizeDag[F, Effect](
              depth,
              (ts, lfb) => GraphzGenerator.dagAsCluster[F, Effect](ts, lfb, config),
              stringify
            )
            .map(graph => VisualizeBlocksResponse(status = "Success", content = graph))
        )
      }

      override def showBlocks(request: BlocksQuery): Observable[BlockInfoWithoutTuplespace] =
        Observable
          .fromTask(defer(BlockAPI.showBlocks[F](request.depth)))
          .flatMap(Observable.fromIterable)

      // TODO: Handle error case
      override def listenForDataAtName(request: DataAtNameQuery): Task[ListeningNameDataResponse] =
        defer(BlockAPI.getListeningNameDataResponse[F](request.depth, request.name.get))

      override def listenForContinuationAtName(
          request: ContinuationAtNameQuery
      ): Task[ListeningNameContinuationResponse] =
        defer(BlockAPI.getListeningNameContinuationResponse[F](request.depth, request.names))

      override def showMainChain(request: BlocksQuery): Observable[BlockInfoWithoutTuplespace] =
        Observable
          .fromTask(defer(BlockAPI.showMainChain[F](request.depth)))
          .flatMap(Observable.fromIterable)

      override def findBlockWithDeploy(request: FindDeployInBlockQuery): Task[BlockQueryResponse] =
        defer(BlockAPI.findBlockWithDeploy[F](request.user, request.timestamp))

      override def previewPrivateNames(
          request: PrivateNamePreviewQuery
      ): Task[PrivateNamePreviewResponse] =
        defer(BlockAPI.previewPrivateNames[F](request.user, request.timestamp, request.nameQty))
    }
}

// this isn't a solution - it's a hack employed to allow confirming that the observed UNKNOWN responses
// are really a side effect of exceptions not properly handled in 'attemptAndLog'
//TODO needs to be replaced with a proper response abstraction which allows errors.
sealed trait ToErrorResponse[A] {
  def fromThrowable(t: Throwable): A
}
object ToErrorResponse {
  def apply[A](implicit ev: ToErrorResponse[A]): ToErrorResponse[A] = ev
}

object ToErrorResponseInstances {
  implicit def listResponseDefaultError[A: ToErrorResponse]: ToErrorResponse[List[A]] =
    new ToErrorResponse[List[A]] {
      override def fromThrowable(t: Throwable): List[A] =
        ToErrorResponse[A].fromThrowable(t) :: Nil
    }

  implicit val deployServiceResponseDefaultError: ToErrorResponse[DeployServiceResponse] =
    new ToErrorResponse[DeployServiceResponse] {
      override def fromThrowable(t: Throwable): DeployServiceResponse =
        DeployServiceResponse(success = false, t.getMessage)
    }

  implicit val blockQueryResponseDefaultError: ToErrorResponse[BlockQueryResponse] =
    new ToErrorResponse[BlockQueryResponse] {
      override def fromThrowable(t: Throwable): BlockQueryResponse =
        BlockQueryResponse(status = t.getMessage)
    }

  implicit val visualizeBlocksResponseDefaultError: ToErrorResponse[VisualizeBlocksResponse] =
    new ToErrorResponse[VisualizeBlocksResponse] {
      override def fromThrowable(t: Throwable): VisualizeBlocksResponse =
        VisualizeBlocksResponse(status = t.getMessage)
    }

  implicit val blockInfoWithoutTuplespaceDefaultError: ToErrorResponse[BlockInfoWithoutTuplespace] =
    new ToErrorResponse[BlockInfoWithoutTuplespace] {
      override def fromThrowable(t: Throwable): BlockInfoWithoutTuplespace =
        BlockInfoWithoutTuplespace()
    }

  implicit val listeningNameDataResponseDefaultError: ToErrorResponse[ListeningNameDataResponse] =
    new ToErrorResponse[ListeningNameDataResponse] {
      override def fromThrowable(t: Throwable): ListeningNameDataResponse =
        ListeningNameDataResponse(status = t.getMessage)
    }

  implicit val listeningNameContinuationResponseDefaultError
    : ToErrorResponse[ListeningNameContinuationResponse] =
    new ToErrorResponse[ListeningNameContinuationResponse] {
      override def fromThrowable(t: Throwable): ListeningNameContinuationResponse =
        ListeningNameContinuationResponse(status = t.getMessage)
    }
  implicit val privateNamePreviewResponseDefaultError: ToErrorResponse[PrivateNamePreviewResponse] =
    new ToErrorResponse[PrivateNamePreviewResponse] {
      override def fromThrowable(t: Throwable): PrivateNamePreviewResponse =
        PrivateNamePreviewResponse()
    }
}
