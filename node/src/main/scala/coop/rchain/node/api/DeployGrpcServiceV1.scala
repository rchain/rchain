package coop.rchain.node.api

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import cats.{Applicative, Foldable}
import com.google.protobuf.ByteString
import coop.rchain.casper.api._
import coop.rchain.casper.protocol._
import coop.rchain.casper.protocol.deploy.v1._
import coop.rchain.catscontrib.TaskContrib.AbstractTaskOps
import coop.rchain.models.StacksafeMessage
import coop.rchain.models.syntax._
import coop.rchain.shared.Log
import coop.rchain.shared.ThrowableOps.RichThrowable
import io.grpc.Metadata
import fs2.Stream

object DeployGrpcServiceV1 {

  def apply[F[_]: Concurrent: Log](
      blockApi: BlockApi[F],
      blockReportAPI: BlockReportApi[F]
  ): DeployServiceFs2Grpc[F, Metadata] =
    new DeployServiceFs2Grpc[F, Metadata] {

      private def defer[A, R <: StacksafeMessage[R]](
          task: F[Either[String, A]]
      )(
          response: Either[ServiceError, A] => R
      ): F[R] =
        task
          .logOnError("Deploy service method error.")
          .attempt
          .map(
            _.fold(
              t => response(ServiceError(t.toMessageList()).asLeft),
              r => response(r.leftMap(e => ServiceError(Seq(e))))
            )
          )

      private def deferCollection[A, R <: StacksafeMessage[R], Collection[_]: Applicative: Foldable](
          task: F[Collection[A]]
      )(
          response: Either[ServiceError, A] => R
      ): F[Collection[R]] =
        task.attempt
          .logOnError("Deploy service method error.")
          .map(
            _.fold(
              t => response(ServiceError(t.toMessageList()).asLeft).pure[Collection],
              _.map(r => response(r.asRight[ServiceError]))
            )
          )

      override def doDeploy(request: DeployDataProto, ctx: Metadata): F[DeployResponse] =
        DeployData
          .from(request)
          .fold(
            errMsg => {
              import DeployResponse.Message._
              val error = ServiceError(Seq[String](errMsg))
              Sync[F].delay(DeployResponse(Error(error)))
            },
            dd => {
              defer(blockApi.deploy(dd)) { r =>
                import DeployResponse.Message
                import DeployResponse.Message._
                DeployResponse(r.fold[Message](Error, Result))
              }
            }
          )

      override def deployStatus(request: FindDeployQuery, ctx: Metadata): F[DeployStatusResponse] =
        defer(blockApi.deployStatus(request.deployId)) { r =>
          import DeployStatusResponse.Message
          import DeployStatusResponse.Message._
          DeployStatusResponse(r.fold[Message](Error, DeployExecStatus))
        }

      override def getBlock(request: BlockQuery, ctx: Metadata): F[BlockResponse] =
        defer(blockApi.getBlock(request.hash)) { r =>
          import BlockResponse.Message
          import BlockResponse.Message._
          BlockResponse(r.fold[Message](Error, BlockInfo))
        }

      override def visualizeDag(
          request: VisualizeDagQuery,
          ctx: Metadata
      ): Stream[F, VisualizeBlocksResponse] =
        Stream
          .eval(
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
          .flatMap(Stream.emits)

      override def machineVerifiableDag(
          request: MachineVerifyQuery,
          ctx: Metadata
      ): F[MachineVerifyResponse] =
        defer(blockApi.machineVerifiableDag(request.depth)) { r =>
          import MachineVerifyResponse.Message
          import MachineVerifyResponse.Message._
          MachineVerifyResponse(r.fold[Message](Error, Content))
        }

      override def getBlocks(request: BlocksQuery, ctx: Metadata): Stream[F, BlockInfoResponse] =
        Stream
          .eval(
            deferCollection(
              blockApi.getBlocks(request.depth).map(_.getOrElse(List.empty[LightBlockInfo]))
            ) { r =>
              import BlockInfoResponse.Message
              import BlockInfoResponse.Message._
              BlockInfoResponse(r.fold[Message](Error, BlockInfo))
            }
          )
          .flatMap(Stream.emits)

      override def listenForDataAtName(
          request: DataAtNameQuery,
          ctx: Metadata
      ): F[ListeningNameDataResponse] =
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

      override def getDataAtName(
          request: DataAtNameByBlockQuery,
          ctx: Metadata
      ): F[RhoDataResponse] =
        defer(blockApi.getDataAtPar(request.par, request.blockHash, request.usePreStateHash)) { r =>
          import RhoDataResponse.Message
          import RhoDataResponse.Message._
          RhoDataResponse(
            r.fold[Message](
              Error, { case (par, block) => Payload(RhoDataPayload(par, block)) }
            )
          )
        }

      override def listenForContinuationAtName(
          request: ContinuationAtNameQuery,
          ctx: Metadata
      ): F[ContinuationAtNameResponse] =
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

      override def findDeploy(request: FindDeployQuery, ctx: Metadata): F[FindDeployResponse] =
        defer(blockApi.findDeploy(request.deployId)) { r =>
          import FindDeployResponse.Message
          import FindDeployResponse.Message._
          FindDeployResponse(r.fold[Message](Error, BlockInfo))
        }

      override def lastFinalizedBlock(
          request: LastFinalizedBlockQuery,
          ctx: Metadata
      ): F[LastFinalizedBlockResponse] =
        defer(blockApi.lastFinalizedBlock) { r =>
          import LastFinalizedBlockResponse.Message
          import LastFinalizedBlockResponse.Message._
          LastFinalizedBlockResponse(r.fold[Message](Error, BlockInfo))
        }

      override def isFinalized(request: IsFinalizedQuery, ctx: Metadata): F[IsFinalizedResponse] =
        defer(blockApi.isFinalized(request.hash)) { r =>
          import IsFinalizedResponse.Message
          import IsFinalizedResponse.Message._
          IsFinalizedResponse(r.fold[Message](Error, IsFinalized))
        }

      override def bondStatus(request: BondStatusQuery, ctx: Metadata): F[BondStatusResponse] =
        defer(blockApi.bondStatus(request.publicKey)) { r =>
          import BondStatusResponse.Message
          import BondStatusResponse.Message._
          BondStatusResponse(r.fold[Message](Error, IsBonded))
        }

      override def exploratoryDeploy(
          request: ExploratoryDeployQuery,
          ctx: Metadata
      ): F[ExploratoryDeployResponse] =
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

      override def getEventByHash(request: ReportQuery, ctx: Metadata): F[EventInfoResponse] =
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

      override def getBlocksByHeights(
          request: BlocksQueryByHeight,
          ctx: Metadata
      ): Stream[F, BlockInfoResponse] =
        Stream
          .eval(
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
          .flatMap(Stream.emits)

      override def status(
          request: com.google.protobuf.empty.Empty,
          ctx: Metadata
      ): F[StatusResponse] =
        blockApi.status.map(StatusResponse().withStatus)
    }
}
