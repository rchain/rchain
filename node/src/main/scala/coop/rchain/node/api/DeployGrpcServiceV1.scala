package coop.rchain.node.api

import cats.data.State
import cats.effect.Concurrent
import cats.mtl.implicits._
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.api._
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol._
import coop.rchain.casper.protocol.deploy.v1._
import coop.rchain.casper.{ProposeFunction, SafetyOracle}
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.codec.Base16
import coop.rchain.graphz._
import coop.rchain.metrics.Span
import coop.rchain.models.StacksafeMessage
import coop.rchain.monix.Monixable
import coop.rchain.shared.Log
import coop.rchain.shared.ThrowableOps._
import coop.rchain.shared.syntax._
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

object DeployGrpcServiceV1 {

  def apply[F[_]: Monixable: Concurrent: Log: SafetyOracle: BlockStore: Span: EngineCell](
      apiMaxBlocksLimit: Int,
      blockReportAPI: BlockReportAPI[F],
      triggerProposeF: Option[ProposeFunction[F]],
      devMode: Boolean = false
  )(
      implicit worker: Scheduler
  ): DeployServiceV1GrpcMonix.DeployService =
    new DeployServiceV1GrpcMonix.DeployService {

      private def defer[A, R <: StacksafeMessage[R]](
          task: F[Either[String, A]]
      )(
          response: Either[ServiceError, A] => R
      ): Task[R] =
        task.toTask
          .executeOn(worker)
          .fromTask
          .logOnError("Deploy service method error.")
          .attempt
          .map(
            _.fold(
              t => response(ServiceError(t.toMessageList()).asLeft),
              r => response(r.leftMap(e => ServiceError(Seq(e))))
            )
          )
          .toTask

      private def deferList[A, R <: StacksafeMessage[R]](task: F[List[A]])(
          response: Either[ServiceError, A] => R
      ): Task[List[R]] =
        task.toTask
          .executeOn(worker)
          .fromTask
          .logOnError("Deploy service method error.")
          .attempt
          .map(
            _.fold(
              t => List(response(ServiceError(t.toMessageList()).asLeft)),
              _.map(r => response(r.asRight[ServiceError]))
            )
          )
          .toTask

      def doDeploy(request: DeployDataProto): Task[DeployResponse] =
        DeployData
          .from(request)
          .fold(
            errMsg => {
              import DeployResponse.Message._
              Task({
                val error = ServiceError(Seq[String](errMsg))
                DeployResponse(Error(error))
              })
            },
            dd => {
              defer(BlockAPI.deploy[F](dd, triggerProposeF)) { r =>
                import DeployResponse.Message
                import DeployResponse.Message._
                DeployResponse(r.fold[Message](Error, Result))
              }
            }
          )

      def getBlock(request: BlockQuery): Task[BlockResponse] =
        defer(BlockAPI.getBlock[F](request.hash)) { r =>
          import BlockResponse.Message
          import BlockResponse.Message._
          BlockResponse(r.fold[Message](Error, BlockInfo))
        }

      def visualizeDag(request: VisualizeDagQuery): Observable[VisualizeBlocksResponse] = {
        type Effect[A] = State[Vector[String], A]
        implicit val ser: GraphSerializer[Effect]             = new ListSerializer[Effect]
        val serialize: Effect[Graphz[Effect]] => List[String] = _.runS(Vector.empty).value.toList

        val depth            = if (request.depth <= 0) apiMaxBlocksLimit else request.depth
        val config           = GraphConfig(request.showJustificationLines)
        val startBlockNumber = request.startBlockNumber

        Observable
          .fromTask(
            deferList(
              BlockAPI
                .visualizeDag[F, Effect, List[String]](
                  depth,
                  apiMaxBlocksLimit,
                  startBlockNumber,
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
        defer(BlockAPI.machineVerifiableDag[F](apiMaxBlocksLimit, apiMaxBlocksLimit)) { r =>
          import MachineVerifyResponse.Message
          import MachineVerifyResponse.Message._
          MachineVerifyResponse(r.fold[Message](Error, Content))
        }

      def showMainChain(request: BlocksQuery): Observable[BlockInfoResponse] =
        Observable
          .fromTask(
            deferList(BlockAPI.showMainChain[F](request.depth, apiMaxBlocksLimit)) { r =>
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
                .getBlocks[F](request.depth, apiMaxBlocksLimit)
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
            .getListeningNameDataResponse[F](request.depth, request.name.get, apiMaxBlocksLimit)
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
            .getListeningNameContinuationResponse[F](
              request.depth,
              request.names,
              apiMaxBlocksLimit
            )
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

      def bondStatus(request: BondStatusQuery): Task[BondStatusResponse] =
        defer(BlockAPI.bondStatus[F](request.publicKey)) { r =>
          import BondStatusResponse.Message
          import BondStatusResponse.Message._
          BondStatusResponse(r.fold[Message](Error, IsBonded))
        }

      def exploratoryDeploy(request: ExploratoryDeployQuery): Task[ExploratoryDeployResponse] =
        defer(
          BlockAPI
            .exploratoryDeploy[F](
              request.term,
              if (request.blockHash.isEmpty) none[String] else Some(request.blockHash),
              request.usePreStateHash,
              devMode
            )
        ) { r =>
          import ExploratoryDeployResponse.Message
          import ExploratoryDeployResponse.Message._
          ExploratoryDeployResponse(r.fold[Message](Error, {
            case (par, block) => Result(DataWithBlockInfo(par, Some(block)))
          }))
        }

      override def getEventByHash(request: ReportQuery): Task[EventInfoResponse] =
        defer(
          Base16
            .decode(request.hash)
            .fold(s"Request hash: ${request.hash} is not valid hex string".asLeft[Array[Byte]])(
              Right(_)
            )
            .flatTraverse(
              hash =>
                blockReportAPI.blockReport(
                  ByteString.copyFrom(hash),
                  request.forceReplay
                )
            )
        ) { r =>
          import EventInfoResponse.Message
          import EventInfoResponse.Message._
          EventInfoResponse(r.fold[Message](Error, Result))
        }

      def getBlocksByHeights(request: BlocksQueryByHeight): Observable[BlockInfoResponse] =
        Observable
          .fromTask(
            deferList(
              BlockAPI
                .getBlocksByHeights[F](
                  request.startBlockNumber,
                  request.endBlockNumber,
                  apiMaxBlocksLimit
                )
                .map(_.getOrElse(List.empty[LightBlockInfo]))
            ) { r =>
              import BlockInfoResponse.Message
              import BlockInfoResponse.Message._
              BlockInfoResponse(r.fold[Message](Error, BlockInfo))
            }
          )
          .flatMap(Observable.fromIterable)
    }
}
