package coop.rchain.casper.api

import scala.collection.immutable

import cats.Monad
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.Semaphore
import cats.implicits._

import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.engine._
import EngineCell._
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper._
import coop.rchain.casper.DeployError._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{DagOperations, EventConverter, ProtoUtil}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.graphz._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.implicits._
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.{BlockMetadata, Par}
import coop.rchain.models.rholang.sorter.Sortable._
import coop.rchain.models.serialization.implicits.mkProtobufInstance
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rspace.StableHashProvider
import coop.rchain.rspace.trace._
import coop.rchain.shared.Log

import com.google.protobuf.ByteString

object BlockAPI {

  type Error           = String
  type ApiErr[A]       = Either[Error, A]
  type Effect[F[_], A] = F[ApiErr[A]]

  val BlockAPIMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "block-api")
  val CreateBlockSource: Metrics.Source     = Metrics.Source(BlockAPIMetricsSource, "create-block")
  val DeploySource: Metrics.Source          = Metrics.Source(BlockAPIMetricsSource, "deploy")
  val GetBlockSource: Metrics.Source        = Metrics.Source(BlockAPIMetricsSource, "get-block")

  def deploy[F[_]: Monad: EngineCell: Log: Span](
      d: DeployData
  ): Effect[F, DeployServiceResponse] = Span[F].trace(DeploySource) {

    def casperDeploy(casper: MultiParentCasper[F]): Effect[F, DeployServiceResponse] =
      casper
        .deploy(d)
        .map(
          _.bimap(
            err => err.show,
            res =>
              DeployServiceResponse(
                s"Success!\nDeployId is: ${PrettyPrinter.buildStringNoLimit(res)}"
              )
          )
        )

    val errorMessage = "Could not deploy, casper instance was not available yet."

    EngineCell[F].read >>= (_.withCasper[ApiErr[DeployServiceResponse]](
      casperDeploy,
      Log[F].warn(errorMessage).as(s"Error: $errorMessage".asLeft)
    ))
  }

  def createBlock[F[_]: Sync: Concurrent: EngineCell: Log: Metrics: Span](
      blockApiLock: Semaphore[F],
      printUnmatchedSends: Boolean = false
  ): Effect[F, DeployServiceResponse] = Span[F].trace(CreateBlockSource) {
    val errorMessage = "Could not create block, casper instance was not available yet."
    EngineCell[F].read >>= (
      _.withCasper[ApiErr[DeployServiceResponse]](
        casper => {
          Sync[F].bracket(blockApiLock.tryAcquire) {
            case true =>
              implicit val ms = BlockAPIMetricsSource
              (for {
                _          <- Metrics[F].incrementCounter("propose")
                maybeBlock <- casper.createBlock
                result <- maybeBlock match {
                           case err: NoBlock =>
                             Metrics[F]
                               .incrementCounter("propose-failed") >> s"Error while creating block: $err"
                               .asLeft[DeployServiceResponse]
                               .pure[F]
                           case Created(block) =>
                             casper
                               .addBlock(block, ignoreDoppelgangerCheck[F]) >>= (addResponse(
                               _,
                               block,
                               casper,
                               printUnmatchedSends
                             ))
                         }
              } yield result).timer("propose-total-time").attempt.flatMap {
                case Left(e) =>
                  Metrics[F].incrementCounter("propose-failed") >> Sync[F]
                    .raiseError[ApiErr[DeployServiceResponse]](e)
                case Right(result) => result.pure[F]
              }
            case false =>
              "Error: There is another propose in progress.".asLeft[DeployServiceResponse].pure[F]
          } {
            case true =>
              blockApiLock.release
            case false =>
              ().pure[F]
          }
        },
        default = Log[F]
          .warn(errorMessage)
          .as(s"Error: $errorMessage".asLeft)
      )
    )
  }

  def getListeningNameDataResponse[F[_]: Concurrent: EngineCell: Log: SafetyOracle: BlockStore](
      depth: Int,
      listeningName: Par
  ): Effect[F, ListeningNameDataResponse] = {

    val errorMessage = "Could not get listening name data, casper instance was not available yet."

    def casperResponse(
        implicit casper: MultiParentCasper[F]
    ): Effect[F, ListeningNameDataResponse] =
      for {
        mainChain           <- getMainChainFromTip[F](depth)
        runtimeManager      <- casper.getRuntimeManager
        sortedListeningName <- parSortable.sortMatch[F](listeningName).map(_.term)
        maybeBlocksWithActiveName <- mainChain.toList.traverse { block =>
                                      getDataWithBlockInfo[F](
                                        runtimeManager,
                                        sortedListeningName,
                                        block
                                      )
                                    }
        blocksWithActiveName = maybeBlocksWithActiveName.flatten
      } yield ListeningNameDataResponse(
        blockResults = blocksWithActiveName,
        length = blocksWithActiveName.length
      ).asRight

    EngineCell[F].read >>= (_.withCasper[ApiErr[ListeningNameDataResponse]](
      casperResponse(_),
      Log[F]
        .warn(errorMessage)
        .as(s"Error: $errorMessage".asLeft)
    ))
  }

  def getListeningNameContinuationResponse[F[_]: Concurrent: EngineCell: Log: SafetyOracle: BlockStore](
      depth: Int,
      listeningNames: Seq[Par]
  ): Effect[F, ListeningNameContinuationResponse] = {
    val errorMessage =
      "Could not get listening names continuation, casper instance was not available yet."
    def casperResponse(
        implicit casper: MultiParentCasper[F]
    ): Effect[F, ListeningNameContinuationResponse] =
      for {
        mainChain      <- getMainChainFromTip[F](depth)
        runtimeManager <- casper.getRuntimeManager
        sortedListeningNames <- listeningNames.toList
                                 .traverse(parSortable.sortMatch[F](_).map(_.term))
        maybeBlocksWithActiveName <- mainChain.toList.traverse { block =>
                                      getContinuationsWithBlockInfo[F](
                                        runtimeManager,
                                        sortedListeningNames,
                                        block
                                      )
                                    }
        blocksWithActiveName = maybeBlocksWithActiveName.flatten
      } yield ListeningNameContinuationResponse(
        blockResults = blocksWithActiveName,
        length = blocksWithActiveName.length
      ).asRight

    EngineCell[F].read >>= (_.withCasper[ApiErr[ListeningNameContinuationResponse]](
      casperResponse(_),
      Log[F]
        .warn(errorMessage)
        .as(s"Error: $errorMessage".asLeft)
    ))
  }

  private def getMainChainFromTip[F[_]: Sync: Log: SafetyOracle: BlockStore](depth: Int)(
      implicit casper: MultiParentCasper[F]
  ): F[IndexedSeq[BlockMessage]] =
    for {
      dag       <- casper.blockDag
      tipHashes <- casper.estimator(dag)
      tipHash   = tipHashes.head
      tip       <- ProtoUtil.getBlock[F](tipHash)
      mainChain <- ProtoUtil.getMainChainUntilDepth[F](tip, IndexedSeq.empty[BlockMessage], depth)
    } yield mainChain

  private def getDataWithBlockInfo[F[_]: Log: SafetyOracle: BlockStore: Concurrent](
      runtimeManager: RuntimeManager[F],
      sortedListeningName: Par,
      block: BlockMessage
  )(implicit casper: MultiParentCasper[F]): F[Option[DataWithBlockInfo]] =
    if (isListeningNameReduced(block, immutable.Seq(sortedListeningName))) {
      val stateHash = ProtoUtil.tuplespace(block)
      for {
        data      <- runtimeManager.getData(stateHash)(sortedListeningName)
        blockInfo <- getLightBlockInfo[F](block)
      } yield Option[DataWithBlockInfo](DataWithBlockInfo(data, Some(blockInfo)))
    } else {
      none[DataWithBlockInfo].pure[F]
    }

  private def getContinuationsWithBlockInfo[F[_]: Log: SafetyOracle: BlockStore: Concurrent](
      runtimeManager: RuntimeManager[F],
      sortedListeningNames: immutable.Seq[Par],
      block: BlockMessage
  )(implicit casper: MultiParentCasper[F]): F[Option[ContinuationsWithBlockInfo]] =
    if (isListeningNameReduced(block, sortedListeningNames)) {
      val stateHash =
        ProtoUtil.tuplespace(block)
      for {
        continuations <- runtimeManager.getContinuation(stateHash)(sortedListeningNames)
        continuationInfos = continuations.map(
          continuation => WaitingContinuationInfo(continuation._1, Some(continuation._2))
        )
        blockInfo <- getLightBlockInfo[F](block)
      } yield Option[ContinuationsWithBlockInfo](
        ContinuationsWithBlockInfo(continuationInfos, Some(blockInfo))
      )
    } else {
      none[ContinuationsWithBlockInfo].pure[F]
    }

  private def isListeningNameReduced(
      block: BlockMessage,
      sortedListeningName: immutable.Seq[Par]
  ): Boolean = {
    val serializedLog = for {
      pd    <- block.body.deploys
      event <- pd.deployLog
    } yield event
    val log =
      serializedLog.map(EventConverter.toRspaceEvent)
    log.exists {
      case Produce(channelHash, _, _, _) =>
        channelHash == StableHashProvider.hash(sortedListeningName)
      case Consume(channelsHashes, _, _, _) =>
        channelsHashes.toList.sorted == sortedListeningName
          .map(StableHashProvider.hash(_))
          .toList
          .sorted
      case COMM(consume, produces, _) =>
        (consume.channelsHashes.toList.sorted ==
          sortedListeningName.map(StableHashProvider.hash(_)).toList.sorted) ||
          produces.exists(
            produce => produce.channelsHash == StableHashProvider.hash(sortedListeningName)
          )
    }
  }

  private def toposortDag[
      F[_]: Monad: EngineCell: Log: SafetyOracle: BlockStore,
      A
  ](maybeDepth: Option[Int])(
      doIt: (MultiParentCasper[F], Vector[Vector[BlockHash]]) => F[ApiErr[A]]
  ): Effect[F, A] = {

    val errorMessage =
      "Could not visualize graph, casper instance was not available yet."

    def casperResponse(implicit casper: MultiParentCasper[F]): Effect[F, A] =
      for {
        dag      <- MultiParentCasper[F].blockDag
        depth    <- maybeDepth.fold(dag.topoSort(0L).map(_.length - 1))(_.pure[F])
        topoSort <- dag.topoSortTail(depth)
        result   <- doIt(casper, topoSort)
      } yield result

    EngineCell[F].read >>= (_.withCasper[ApiErr[A]](
      casperResponse(_),
      Log[F].warn(errorMessage).as(errorMessage.asLeft)
    ))
  }

  def visualizeDag[
      F[_]: Monad: Sync: EngineCell: Log: SafetyOracle: BlockStore,
      G[_]: Monad: GraphSerializer,
      R
  ](
      depth: Option[Int],
      visualizer: (Vector[Vector[BlockHash]], String) => F[G[Graphz[G]]],
      serialize: G[Graphz[G]] => R
  ): Effect[F, R] =
    toposortDag[F, R](depth) {
      case (casper, topoSort) =>
        for {
          lfb   <- casper.lastFinalizedBlock
          graph <- visualizer(topoSort, PrettyPrinter.buildString(lfb.blockHash))
        } yield serialize(graph).asRight[Error]
    }

  def machineVerifiableDag[
      F[_]: Monad: Sync: EngineCell: Log: SafetyOracle: BlockStore
  ]: Effect[F, MachineVerifyResponse] =
    toposortDag[F, MachineVerifyResponse](maybeDepth = None) {
      case (_, topoSort) =>
        val fetchParents: BlockHash => F[List[BlockHash]] = { blockHash =>
          ProtoUtil.getBlock[F](blockHash) map (_.header.parentsHashList)
        }

        MachineVerifiableDag[F](topoSort, fetchParents)
          .map(_.map(edges => edges.show).mkString("\n"))
          .map(MachineVerifyResponse(_).asRight[Error])
    }

  def getBlocks[F[_]: Sync: EngineCell: Log: SafetyOracle: BlockStore](
      depth: Option[Int]
  ): Effect[F, List[LightBlockInfo]] =
    toposortDag[F, List[LightBlockInfo]](depth) {
      case (casper, topoSort) =>
        implicit val ev: MultiParentCasper[F] = casper
        topoSort
          .foldM(List.empty[LightBlockInfo]) {
            case (blockInfosAtHeightAcc, blockHashesAtHeight) =>
              for {
                blocksAtHeight <- blockHashesAtHeight.traverse(ProtoUtil.getBlock[F])
                blockInfosAtHeight <- blocksAtHeight.traverse(
                                       getLightBlockInfo[F]
                                     )
              } yield blockInfosAtHeightAcc ++ blockInfosAtHeight
          }
          .map(_.reverse.asRight[Error])
    }

  def showMainChain[F[_]: Sync: EngineCell: Log: SafetyOracle: BlockStore](
      depth: Int
  ): F[List[LightBlockInfo]] = {

    val errorMessage =
      "Could not show main chain, casper instance was not available yet."

    def casperResponse(implicit casper: MultiParentCasper[F]) =
      for {
        dag        <- MultiParentCasper[F].blockDag
        tipHashes  <- MultiParentCasper[F].estimator(dag)
        tipHash    = tipHashes.head
        tip        <- ProtoUtil.getBlock[F](tipHash)
        mainChain  <- ProtoUtil.getMainChainUntilDepth[F](tip, IndexedSeq.empty[BlockMessage], depth)
        blockInfos <- mainChain.toList.traverse(getLightBlockInfo[F])
      } yield blockInfos

    EngineCell[F].read >>= (_.withCasper[List[LightBlockInfo]](
      casperResponse(_),
      Log[F].warn(errorMessage).as(List.empty[LightBlockInfo])
    ))
  }

  def findDeploy[F[_]: Sync: EngineCell: Log: SafetyOracle: BlockStore](
      id: DeployId
  ): Effect[F, LightBlockQueryResponse] =
    EngineCell[F].read >>= (
      _.withCasper[ApiErr[LightBlockQueryResponse]](
        implicit casper =>
          for {
            dag            <- casper.blockDag
            maybeBlockHash <- dag.lookupByDeployId(id)
            maybeBlock     <- maybeBlockHash.traverse(ProtoUtil.getBlock[F])
            response       <- maybeBlock.traverse(getLightBlockInfo[F])
          } yield response.fold(
            s"Couldn't find block containing deploy with id: ${PrettyPrinter
              .buildStringNoLimit(id)}".asLeft[LightBlockQueryResponse]
          )(
            blockInfo =>
              LightBlockQueryResponse(
                blockInfo = Some(blockInfo)
              ).asRight
          ),
        Log[F]
          .warn("Could not find block with deploy, casper instance was not available yet.")
          .as(s"Error: errorMessage".asLeft)
      )
    )

  def getBlock[F[_]: Monad: EngineCell: Log: SafetyOracle: BlockStore: Span](
      q: BlockQuery
  ): Effect[F, BlockQueryResponse] = Span[F].trace(GetBlockSource) {

    val errorMessage =
      "Could not get block, casper instance was not available yet."

    def casperResponse(
        implicit casper: MultiParentCasper[F]
    ): Effect[F, BlockQueryResponse] =
      for {
        dag        <- MultiParentCasper[F].blockDag
        maybeBlock <- getBlock[F](q, dag)
        blockQueryResponse <- maybeBlock match {
                               case Some(block) =>
                                 for {
                                   blockInfo <- getFullBlockInfo[F](block)
                                 } yield BlockQueryResponse(blockInfo = Some(blockInfo))
                                   .asRight[Error]
                               case None =>
                                 s"Error: Failure to find block with hash ${q.hash}"
                                   .asLeft[BlockQueryResponse]
                                   .pure[F]
                             }
      } yield blockQueryResponse

    EngineCell[F].read >>= (_.withCasper[ApiErr[BlockQueryResponse]](
      casperResponse(_),
      Log[F].warn(errorMessage).as(s"Error: $errorMessage".asLeft)
    ))
  }

  private def getBlockInfo[A, F[_]: Monad: SafetyOracle: BlockStore](
      block: BlockMessage,
      constructor: (
          BlockMessage,
          Long,
          Int,
          BlockHash,
          Long,
          BlockHash,
          Seq[BlockHash],
          Float,
          Float,
          Seq[Bond],
          Seq[ProcessedDeploy]
      ) => F[A]
  )(implicit casper: MultiParentCasper[F]): F[A] =
    for {
      dag             <- casper.blockDag
      header          = block.header
      version         = header.version
      deployCount     = header.deployCount
      tsHash          = ProtoUtil.tuplespace(block)
      timestamp       = header.timestamp
      mainParent      = header.parentsHashList.headOption.getOrElse(ByteString.EMPTY)
      parentsHashList = header.parentsHashList
      normalizedFaultTolerance <- SafetyOracle[F]
                                   .normalizedFaultTolerance(dag, block.blockHash) // TODO: Warn about parent block finalization
      initialFault       <- casper.normalizedInitialFault(ProtoUtil.weightMap(block))
      bondsValidatorList = ProtoUtil.bonds(block)
      processedDeploy    = ProtoUtil.deploys(block)
      blockInfo <- constructor(
                    block,
                    version,
                    deployCount,
                    tsHash,
                    timestamp,
                    mainParent,
                    parentsHashList,
                    normalizedFaultTolerance,
                    initialFault,
                    bondsValidatorList,
                    processedDeploy
                  )
    } yield blockInfo

  private def getFullBlockInfo[F[_]: Monad: SafetyOracle: BlockStore](
      block: BlockMessage
  )(implicit casper: MultiParentCasper[F]): F[BlockInfo] =
    getBlockInfo[BlockInfo, F](block, constructBlockInfo[F])
  private def getLightBlockInfo[F[_]: Monad: SafetyOracle: BlockStore](
      block: BlockMessage
  )(implicit casper: MultiParentCasper[F]): F[LightBlockInfo] =
    getBlockInfo[LightBlockInfo, F](block, constructLightBlockInfo[F])

  private def constructBlockInfo[F[_]: Monad: SafetyOracle: BlockStore](
      block: BlockMessage,
      version: Long,
      deployCount: Int,
      tsHash: BlockHash,
      timestamp: Long,
      mainParent: BlockHash,
      parentsHashList: Seq[BlockHash],
      normalizedFaultTolerance: Float,
      initialFault: Float,
      bondsValidatorList: Seq[Bond],
      processedDeploys: Seq[ProcessedDeploy]
  ): F[BlockInfo] =
    BlockInfo(
      blockHash = PrettyPrinter.buildStringNoLimit(block.blockHash),
      blockSize = BlockMessage.toProto(block).serializedSize.toString,
      blockNumber = ProtoUtil.blockNumber(block),
      version = version,
      deployCount = deployCount,
      tupleSpaceHash = PrettyPrinter.buildStringNoLimit(tsHash),
      timestamp = timestamp,
      faultTolerance = normalizedFaultTolerance - initialFault, // TODO: Fix
      mainParentHash = PrettyPrinter.buildStringNoLimit(mainParent),
      parentsHashList = parentsHashList.map(PrettyPrinter.buildStringNoLimit),
      sender = PrettyPrinter.buildStringNoLimit(block.sender),
      shardId = block.shardId,
      bondsValidatorList = bondsValidatorList.map(PrettyPrinter.buildString),
      deployCost = processedDeploys.map(PrettyPrinter.buildString)
    ).pure[F]

  private def constructLightBlockInfo[F[_]: Monad: SafetyOracle: BlockStore](
      block: BlockMessage,
      version: Long,
      deployCount: Int,
      tsHash: BlockHash,
      timestamp: Long,
      mainParent: BlockHash,
      parentsHashList: Seq[BlockHash],
      normalizedFaultTolerance: Float,
      initialFault: Float,
      bondsValidatorList: Seq[Bond],
      processedDeploys: Seq[ProcessedDeploy]
  ): F[LightBlockInfo] =
    LightBlockInfo(
      blockHash = PrettyPrinter.buildStringNoLimit(block.blockHash),
      blockSize = block.toProto.serializedSize.toString,
      blockNumber = ProtoUtil.blockNumber(block),
      version = version,
      deployCount = deployCount,
      tupleSpaceHash = PrettyPrinter.buildStringNoLimit(tsHash),
      timestamp = timestamp,
      faultTolerance = normalizedFaultTolerance - initialFault,
      mainParentHash = PrettyPrinter.buildStringNoLimit(mainParent),
      parentsHashList = parentsHashList.map(PrettyPrinter.buildStringNoLimit),
      sender = PrettyPrinter.buildStringNoLimit(block.sender)
    ).pure[F]

  private def getBlock[F[_]: Monad: BlockStore](
      q: BlockQuery,
      dag: BlockDagRepresentation[F]
  ): F[Option[BlockMessage]] =
    for {
      findResult <- BlockStore[F].find(h => Base16.encode(h.toByteArray).startsWith(q.hash))
    } yield findResult.headOption match {
      case Some((_, block)) => Some(block)
      case None             => none[BlockMessage]
    }

  private def addResponse[F[_]: Concurrent](
      status: ValidBlockProcessing,
      block: BlockMessage,
      casper: MultiParentCasper[F],
      printUnmatchedSends: Boolean
  ): Effect[F, DeployServiceResponse] =
    status
      .map { _ =>
        val hash    = PrettyPrinter.buildString(block.blockHash)
        val deploys = block.body.deploys.map(_.deploy)
        val maybeUnmatchedSendsOutputF =
          if (printUnmatchedSends) prettyPrintUnmatchedSends(casper, deploys).map(_.some)
          else none[String].pure[F]
        maybeUnmatchedSendsOutputF >>= (
            maybeOutput =>
              DeployServiceResponse(
                s"Success! Block $hash created and added.${maybeOutput.map("\n" + _).getOrElse("")}"
              ).asRight[Error].pure[F]
          )
      }
      .leftMap {
        case _: InvalidBlock =>
          s"Failure! Invalid block: $status".asLeft[DeployServiceResponse].pure[F]
        case BlockError.BlockException(ex) =>
          s"Error during block processing: $ex".asLeft[DeployServiceResponse].pure[F]
        case BlockError.Processing =>
          "No action taken since other thread is already processing the block."
            .asLeft[DeployServiceResponse]
            .pure[F]
      }
      .merge

  private def prettyPrintUnmatchedSends[F[_]: Concurrent](
      casper: MultiParentCasper[F],
      deploys: Seq[DeployData]
  ): F[String] =
    casper.getRuntimeManager >>= (
      _.withRuntimeLock(runtime => StoragePrinter.prettyPrintUnmatchedSends(deploys, runtime))
    )

  def previewPrivateNames[F[_]: Monad: Log](
      deployer: ByteString,
      timestamp: Long,
      nameQty: Int
  ): Effect[F, PrivateNamePreviewResponse] = {
    val seed    = DeployDataProto().withDeployer(deployer).withTimestamp(timestamp)
    val rand    = Blake2b512Random(DeployDataProto.toByteArray(seed))
    val safeQty = nameQty min 1024
    val ids     = (0 until safeQty).map(_ => ByteString.copyFrom(rand.next()))
    PrivateNamePreviewResponse(ids).asRight[String].pure[F]
  }

  def lastFinalizedBlock[F[_]: Monad: EngineCell: SafetyOracle: BlockStore: Log]
      : Effect[F, LastFinalizedBlockResponse] = {
    val errorMessage = "Could not get last finalized block, casper instance was not available yet."
    EngineCell[F].read >>= (
      _.withCasper[ApiErr[LastFinalizedBlockResponse]](
        implicit casper =>
          for {
            lastFinalizedBlock <- casper.lastFinalizedBlock
            blockInfo          <- getFullBlockInfo[F](lastFinalizedBlock)
          } yield LastFinalizedBlockResponse(blockInfo = Some(blockInfo)).asRight,
        Log[F].warn(errorMessage).as(s"Error: $errorMessage".asLeft)
      )
    )
  }

  def isFinalized[F[_]: Monad: EngineCell: SafetyOracle: BlockStore: Log](
      request: IsFinalizedQuery
  ): Effect[F, IsFinalizedResponse] = {
    val errorMessage =
      "Could not check if block is finalized, casper instance was not available yet."
    EngineCell[F].read >>= (
      _.withCasper[ApiErr[IsFinalizedResponse]](
        implicit casper =>
          for {
            lastFinalizedBlock <- casper.lastFinalizedBlock
            lastFinalizedBlockMetadata = BlockMetadata
              .fromBlock(lastFinalizedBlock, invalid = false)
            dag                   <- casper.blockDag
            givenBlockHash        = ProtoUtil.stringToByteString(request.hash)
            givenBlockMetadataOpt <- dag.lookup(givenBlockHash)
            result <- givenBlockMetadataOpt match {
                       case None =>
                         s"Could not find block with hash ${request.hash}"
                           .asLeft[IsFinalizedResponse]
                           .pure[F]
                       case Some(givenBlockMetadata) =>
                         DagOperations
                           .bfTraverseF(List(lastFinalizedBlockMetadata)) { b =>
                             b.parents.traverse(dag.lookup).map { parentOpts =>
                               parentOpts.flatten.distinct
                                 .filter(_.blockNum >= givenBlockMetadata.blockNum)
                             }
                           }
                           .contains(givenBlockMetadata)
                           .map(
                             isContained =>
                               IsFinalizedResponse(
                                 isFinalized = isContained
                               ).asRight[Error]
                           )
                     }
          } yield result,
        Log[F].warn(errorMessage).as(s"Error: $errorMessage".asLeft)
      )
    )
  }
}
