package coop.rchain.node.api

import cats.effect.Concurrent
import cats.syntax.all._
import cats.{Applicative, Foldable}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.casper.api._
import coop.rchain.casper.protocol._
import coop.rchain.casper.protocol.deploy.v1._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.models.StacksafeMessage
import coop.rchain.models.syntax._
import coop.rchain.monix.Monixable
import coop.rchain.shared.Log
import coop.rchain.shared.ThrowableOps._
import coop.rchain.shared.syntax._
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable

object DeployGrpcServiceV1 {

  def apply[F[_]: Monixable: Concurrent: BlockStore: RPConfAsk: ConnectionsCell: NodeDiscovery: Log](
      blockApi: BlockAPI_v2[F],
      blockReportAPI: BlockReportAPI[F]
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

      private def deferCollection[A, R <: StacksafeMessage[R], Collection[_]: Applicative: Foldable](
          task: F[Collection[A]]
      )(
          response: Either[ServiceError, A] => R
      ): Task[Collection[R]] =
        task.toTask
          .executeOn(worker)
          .fromTask
          .logOnError("Deploy service method error.")
          .attempt
          .map(
            _.fold(
              t => response(ServiceError(t.toMessageList()).asLeft).pure[Collection],
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
              defer(blockApi.deploy(dd)) { r =>
                import DeployResponse.Message
                import DeployResponse.Message._
                DeployResponse(r.fold[Message](Error, Result))
              }
            }
          )

      def getBlock(request: BlockQuery): Task[BlockResponse] =
        defer(blockApi.getBlock(request.hash)) { r =>
          import BlockResponse.Message
          import BlockResponse.Message._
          BlockResponse(r.fold[Message](Error, BlockInfo))
        }

      def visualizeDag(request: VisualizeDagQuery): Observable[VisualizeBlocksResponse] =
        Observable
          .fromTask(
            deferCollection(
              blockApi
                .visualizeDag(
                  request.depth,
                  request.startBlockNumber,
                  request.showJustificationLines
                )
                .map(x => x.getOrElse(Vector.empty[String]))
            ) { r =>
              import VisualizeBlocksResponse.Message
              import VisualizeBlocksResponse.Message._
              VisualizeBlocksResponse(r.fold[Message](Error, Content))
            }
          )
          .flatMap(Observable.fromIterable)

      def machineVerifiableDag(request: MachineVerifyQuery): Task[MachineVerifyResponse] =
        defer(blockApi.machineVerifiableDag(request.depth)) { r =>
          import MachineVerifyResponse.Message
          import MachineVerifyResponse.Message._
          MachineVerifyResponse(r.fold[Message](Error, Content))
        }

      def getBlocks(request: BlocksQuery): Observable[BlockInfoResponse] =
        Observable
          .fromTask(
            deferCollection(
              blockApi.getBlocks(request.depth).map(_.getOrElse(List.empty[LightBlockInfo]))
            ) { r =>
              import BlockInfoResponse.Message
              import BlockInfoResponse.Message._
              BlockInfoResponse(r.fold[Message](Error, BlockInfo))
            }
          )
          .flatMap(Observable.fromIterable)

      def listenForDataAtName(request: DataAtNameQuery): Task[ListeningNameDataResponse] =
        defer(
          blockApi.getListeningNameDataResponse(request.depth, request.name)
        ) { r =>
          import ListeningNameDataResponse.Message
          import ListeningNameDataResponse.Message._
          ListeningNameDataResponse(
            r.fold[Message](
              Error, { case (br, l) => Payload(ListeningNameDataPayload(br, l)) }
            )
          )
        }

      def getDataAtName(request: DataAtNameByBlockQuery): Task[RhoDataResponse] =
        defer(blockApi.getDataAtPar(request.par, request.blockHash, request.usePreStateHash)) { r =>
          import RhoDataResponse.Message
          import RhoDataResponse.Message._
          RhoDataResponse(
            r.fold[Message](
              Error, { case (par, block) => Payload(RhoDataPayload(par, block)) }
            )
          )
        }

      def listenForContinuationAtName(
          request: ContinuationAtNameQuery
      ): Task[ContinuationAtNameResponse] =
        defer(
          blockApi.getListeningNameContinuationResponse(request.depth, request.names)
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
        defer(blockApi.findDeploy(request.deployId)) { r =>
          import FindDeployResponse.Message
          import FindDeployResponse.Message._
          FindDeployResponse(r.fold[Message](Error, BlockInfo))
        }

      def previewPrivateNames(request: PrivateNamePreviewQuery): Task[PrivateNamePreviewResponse] =
        defer(blockApi.previewPrivateNames(request.user, request.timestamp, request.nameQty)) { r =>
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
        defer(blockApi.lastFinalizedBlock) { r =>
          import LastFinalizedBlockResponse.Message
          import LastFinalizedBlockResponse.Message._
          LastFinalizedBlockResponse(r.fold[Message](Error, BlockInfo))
        }

      def isFinalized(request: IsFinalizedQuery): Task[IsFinalizedResponse] =
        defer(blockApi.isFinalized(request.hash)) { r =>
          import IsFinalizedResponse.Message
          import IsFinalizedResponse.Message._
          IsFinalizedResponse(r.fold[Message](Error, IsFinalized))
        }

      def bondStatus(request: BondStatusQuery): Task[BondStatusResponse] =
        defer(blockApi.bondStatus(request.publicKey)) { r =>
          import BondStatusResponse.Message
          import BondStatusResponse.Message._
          BondStatusResponse(r.fold[Message](Error, IsBonded))
        }

      def exploratoryDeploy(request: ExploratoryDeployQuery): Task[ExploratoryDeployResponse] =
        defer(
          blockApi
            .exploratoryDeploy(
              request.term,
              if (request.blockHash.isEmpty) none[String] else Some(request.blockHash),
              request.usePreStateHash
            )
        ) { r =>
          import ExploratoryDeployResponse.Message
          import ExploratoryDeployResponse.Message._
          ExploratoryDeployResponse(r.fold[Message](Error, {
            case (par, block) => Result(DataWithBlockInfo(par, block))
          }))
        }

      override def getEventByHash(request: ReportQuery): Task[EventInfoResponse] =
        defer(
          request.hash.decodeHex
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
            deferCollection(
              blockApi
                .getBlocksByHeights(request.startBlockNumber, request.endBlockNumber)
                .map(_.getOrElse(List.empty[LightBlockInfo]))
            ) { r =>
              import BlockInfoResponse.Message
              import BlockInfoResponse.Message._
              BlockInfoResponse(r.fold[Message](Error, BlockInfo))
            }
          )
          .flatMap(Observable.fromIterable)

      def status(request: com.google.protobuf.empty.Empty): Task[StatusResponse] =
        blockApi.status.map(StatusResponse().withStatus).toTask
    }
}
