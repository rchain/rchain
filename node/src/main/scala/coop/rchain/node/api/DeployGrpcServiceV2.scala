package coop.rchain.node.api

import cats.data.StateT
import cats.effect.Concurrent
import cats.effect.concurrent.Semaphore
import cats.implicits._
import cats.Id
import cats.mtl.implicits._

import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.deployV2._
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.api._
import coop.rchain.casper.protocol.{
  DeployServiceResponse => _,
  IsFinalizedResponse => _,
  LastFinalizedBlockResponse => _,
  ListeningNameDataResponse => _,
  MachineVerifyResponse => _,
  PrivateNamePreviewResponse => _,
  VisualizeBlocksResponse => _,
  _
}
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.Taskable
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.graphz._
import coop.rchain.metrics.Span
import coop.rchain.models.StacksafeMessage
import coop.rchain.shared.Log
import coop.rchain.shared.ThrowableOps._

import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

object DeployGrpcServiceV2 {
  def instance[F[_]: Concurrent: Log: SafetyOracle: BlockStore: Taskable: Span: EngineCell](
      blockApiLock: Semaphore[F]
  )(
      implicit worker: Scheduler
  ): DeployServiceV2GrpcMonix.DeployServiceV2 =
    new DeployServiceV2GrpcMonix.DeployServiceV2 {

      private def defer[A, R <: StacksafeMessage[R]](
          task: F[Either[String, A]]
      )(
          response: Either[ServiceError, A] => R
      ): Task[R] =
        Task
          .defer(task.toTask)
          .executeOn(worker)
          .attemptAndLog
          .attempt
          .map(
            _.fold(
              t => response(ServiceError(t.toMessageList()).asLeft),
              r => response(r.leftMap(e => ServiceError(Seq(e))))
            )
          )

      private def deferList[A, R <: StacksafeMessage[R]](task: F[List[A]])(
          response: Either[ServiceError, A] => R
      ): Task[List[R]] =
        Task
          .defer(task.toTask)
          .executeOn(worker)
          .attemptAndLog
          .attempt
          .map(
            _.fold(
              t => List(response(ServiceError(t.toMessageList()).asLeft)),
              _.map(r => response(r.asRight[ServiceError]))
            )
          )

      def doDeploy(request: DeployData): Task[DeployResponse] =
        defer(BlockAPI.deploy[F](request)) { r =>
          import DeployResponse.Message
          import DeployResponse.Message._
          DeployResponse(r.fold[Message](Error, Result))
        }

      def getBlock(request: BlockQuery): Task[BlockResponse] =
        defer(BlockAPI.getBlock[F](request.hash)) { r =>
          import BlockResponse.Message
          import BlockResponse.Message._
          BlockResponse(r.fold[Message](Error, BlockInfo))
        }

      def visualizeDag(request: VisualizeDagQuery): Observable[VisualizeBlocksResponse] = {
        type Effect[A] = StateT[Id, Vector[String], A]
        implicit val ser: GraphSerializer[Effect]             = new ListSerializer[Effect]
        val serialize: Effect[Graphz[Effect]] => List[String] = _.runS(Vector.empty).toList

        val depth  = if (request.depth <= 0) None else Some(request.depth)
        val config = GraphConfig(request.showJustificationLines)

        Observable
          .fromTask(
            deferList(
              BlockAPI
                .visualizeDag[F, Effect, List[String]](
                  depth,
                  (ts, lfb) => GraphzGenerator.dagAsCluster[F, Effect](ts, lfb, config),
                  serialize
                )
                .map(_.getOrElse(List.empty[String]))
            ) { r =>
              import VisualizeBlocksResponse.Message
              import VisualizeBlocksResponse.Message._
              VisualizeBlocksResponse(r.fold[Message](Error, Content))
            }
          )
          .flatMap(Observable.fromIterable)
      }

      def machineVerifiableDag(request: MachineVerifyQuery): Task[MachineVerifyResponse] =
        defer(BlockAPI.machineVerifiableDag[F]) { r =>
          import MachineVerifyResponse.Message
          import MachineVerifyResponse.Message._
          MachineVerifyResponse(r.fold[Message](Error, Content))
        }

      def showMainChain(request: BlocksQuery): Observable[BlockInfoResponse] =
        Observable
          .fromTask(
            deferList(BlockAPI.showMainChain[F](request.depth)) { r =>
              import BlockInfoResponse.Message
              import BlockInfoResponse.Message._
              BlockInfoResponse(r.fold[Message](Error, BlockInfo))
            }
          )
          .flatMap(Observable.fromIterable)

      def getBlocks(request: BlocksQuery): Observable[BlockInfoResponse] =
        Observable
          .fromTask(
            deferList(
              BlockAPI
                .getBlocks[F](Some(request.depth))
                .map(_.getOrElse(List.empty[LightBlockInfo]))
            ) { r =>
              import BlockInfoResponse.Message
              import BlockInfoResponse.Message._
              BlockInfoResponse(r.fold[Message](Error, BlockInfo))
            }
          )
          .flatMap(Observable.fromIterable)

      def listenForDataAtName(request: DataAtNameQuery): Task[ListeningNameDataResponse] =
        defer(
          BlockAPI
            .getListeningNameDataResponse[F](request.depth, request.name.get)
        ) { r =>
          import ListeningNameDataResponse.Message
          import ListeningNameDataResponse.Message._
          ListeningNameDataResponse(
            r.fold[Message](
              Error, { case (br, l) => Payload(ListeningNameDataPayload(br, l)) }
            )
          )
        }

      def listenForContinuationAtName(
          request: ContinuationAtNameQuery
      ): Task[ContinuationAtNameResponse] =
        defer(
          BlockAPI
            .getListeningNameContinuationResponse[F](request.depth, request.names)
        ) { r =>
          import ContinuationAtNameResponse.Message
          import ContinuationAtNameResponse.Message._
          ContinuationAtNameResponse(
            r.fold[Message](
              Error, { case (br, l) => Payload(ContinuationAtNamePayload(br, l)) }
            )
          )
        }

      def findDeploy(request: FindDeployQuery): Task[FindDeployResponse] =
        defer(BlockAPI.findDeploy[F](request.deployId)) { r =>
          import FindDeployResponse.Message
          import FindDeployResponse.Message._
          FindDeployResponse(r.fold[Message](Error, BlockInfo))
        }

      def previewPrivateNames(request: PrivateNamePreviewQuery): Task[PrivateNamePreviewResponse] =
        defer(
          BlockAPI
            .previewPrivateNames[F](request.user, request.timestamp, request.nameQty)
        ) { r =>
          import PrivateNamePreviewResponse.Message
          import PrivateNamePreviewResponse.Message._
          PrivateNamePreviewResponse(
            r.fold[Message](
              Error,
              ids => Payload(PrivateNamePreviewPayload(ids))
            )
          )
        }

      def lastFinalizedBlock(request: LastFinalizedBlockQuery): Task[LastFinalizedBlockResponse] =
        defer(BlockAPI.lastFinalizedBlock[F]) { r =>
          import LastFinalizedBlockResponse.Message
          import LastFinalizedBlockResponse.Message._
          LastFinalizedBlockResponse(r.fold[Message](Error, BlockInfo))
        }

      def isFinalized(request: IsFinalizedQuery): Task[IsFinalizedResponse] =
        defer(BlockAPI.isFinalized[F](request.hash)) { r =>
          import IsFinalizedResponse.Message
          import IsFinalizedResponse.Message._
          IsFinalizedResponse(r.fold[Message](Error, IsFinalized))
        }
    }
}
