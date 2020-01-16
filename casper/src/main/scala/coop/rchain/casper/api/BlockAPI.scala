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
import coop.rchain.casper.util._
import coop.rchain.casper.util.rholang.{RuntimeManager, Tools}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.graphz._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.metrics.implicits._
import coop.rchain.models.{BlockMetadata, Par}
import coop.rchain.models.rholang.sorter.Sortable._
import coop.rchain.models.serialization.implicits.mkProtobufInstance
import coop.rchain.models.BlockHash.{BlockHash, _}
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rspace.StableHashProvider
import coop.rchain.rspace.trace._
import coop.rchain.shared.Log
import com.google.protobuf.ByteString
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.Signed

object BlockAPI {

  type Error     = String
  type ApiErr[A] = Either[Error, A]

  val BlockAPIMetricsSource: Metrics.Source = Metrics.Source(Metrics.BaseSource, "block-api")
  val CreateBlockSource: Metrics.Source     = Metrics.Source(BlockAPIMetricsSource, "create-block")
  val DeploySource: Metrics.Source          = Metrics.Source(BlockAPIMetricsSource, "deploy")
  val GetBlockSource: Metrics.Source        = Metrics.Source(BlockAPIMetricsSource, "get-block")

  def deploy[F[_]: Monad: EngineCell: Log: Span](
      d: Signed[DeployData]
  ): F[ApiErr[String]] = Span[F].trace(DeploySource) {

    def casperDeploy(casper: MultiParentCasper[F]): F[ApiErr[String]] =
      casper
        .deploy(d)
        .map(
          _.bimap(
            err => err.show,
            res => s"Success!\nDeployId is: ${PrettyPrinter.buildStringNoLimit(res)}"
          )
        )

    val errorMessage = "Could not deploy, casper instance was not available yet."

    EngineCell[F].read >>= (_.withCasper[ApiErr[String]](
      casperDeploy,
      Log[F].warn(errorMessage).as(s"Error: $errorMessage".asLeft)
    ))
  }

  def createBlock[F[_]: Sync: Concurrent: EngineCell: Log: Metrics: Span](
      blockApiLock: Semaphore[F],
      printUnmatchedSends: Boolean = false
  ): F[ApiErr[String]] = Span[F].trace(CreateBlockSource) {
    val errorMessage = "Could not create block, casper instance was not available yet."
    EngineCell[F].read >>= (
      _.withCasper[ApiErr[String]](
        casper => {
          Sync[F].bracket(blockApiLock.tryAcquire) {
            case true =>
              implicit val ms = BlockAPIMetricsSource
              (for {
                _ <- Metrics[F].incrementCounter("propose")
                // TODO: Get rid off CreateBlockStatus and use EitherT
                maybeBlock <- casper.createBlock
                result <- maybeBlock match {
                           case err: NoBlock =>
                             s"Error while creating block: $err"
                               .asLeft[String]
                               .pure[F]
                           case Created(block) =>
                             Log[F].info(s"Proposing ${PrettyPrinter.buildString(block)}") *>
                               casper
                                 .addBlock(block) >>= (addResponse(
                               _,
                               block,
                               casper,
                               printUnmatchedSends
                             ))
                         }
              } yield result)
                .timer("propose-total-time")
                .attempt
                .map(_.leftMap(e => s"Error while creating block: ${e.getMessage}").joinRight)
                .flatMap {
                  case Left(error) =>
                    Metrics[F].incrementCounter("propose-failed") >>
                      Log[F].warn(error) >>
                      error.asLeft[String].pure[F]
                  case result =>
                    result.pure[F]
                }
            case false =>
              "Error: There is another propose in progress.".asLeft[String].pure[F]
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
  ): F[ApiErr[(Seq[DataWithBlockInfo], Int)]] = {

    val errorMessage = "Could not get listening name data, casper instance was not available yet."

    def casperResponse(
        implicit casper: MultiParentCasper[F]
    ): F[ApiErr[(Seq[DataWithBlockInfo], Int)]] =
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
      } yield (blocksWithActiveName, blocksWithActiveName.length).asRight

    EngineCell[F].read >>= (_.withCasper[ApiErr[(Seq[DataWithBlockInfo], Int)]](
      casperResponse(_),
      Log[F]
        .warn(errorMessage)
        .as(s"Error: $errorMessage".asLeft)
    ))
  }

  def getListeningNameContinuationResponse[F[_]: Concurrent: EngineCell: Log: SafetyOracle: BlockStore](
      depth: Int,
      listeningNames: Seq[Par]
  ): F[ApiErr[(Seq[ContinuationsWithBlockInfo], Int)]] = {
    val errorMessage =
      "Could not get listening names continuation, casper instance was not available yet."
    def casperResponse(
        implicit casper: MultiParentCasper[F]
    ): F[ApiErr[(Seq[ContinuationsWithBlockInfo], Int)]] =
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
      } yield (blocksWithActiveName, blocksWithActiveName.length).asRight

    EngineCell[F].read >>= (_.withCasper[ApiErr[(Seq[ContinuationsWithBlockInfo], Int)]](
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
      val stateHash = ProtoUtil.postStateHash(block)
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
      val stateHash = ProtoUtil.postStateHash(block)
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
      case Produce(channelHash, _, _) =>
        channelHash == StableHashProvider.hash(sortedListeningName)
      case Consume(channelsHashes, _, _) =>
        channelsHashes.toList.sorted == sortedListeningName
          .map(StableHashProvider.hash(_))
          .toList
          .sorted
      case COMM(consume, produces, _, _) =>
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
  ): F[ApiErr[A]] = {

    val errorMessage =
      "Could not visualize graph, casper instance was not available yet."

    def casperResponse(implicit casper: MultiParentCasper[F]): F[ApiErr[A]] =
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
  ): F[ApiErr[R]] =
    toposortDag[F, R](depth) {
      case (casper, topoSort) =>
        for {
          lfb   <- casper.lastFinalizedBlock
          graph <- visualizer(topoSort, PrettyPrinter.buildString(lfb.blockHash))
        } yield serialize(graph).asRight[Error]
    }

  def machineVerifiableDag[
      F[_]: Monad: Sync: EngineCell: Log: SafetyOracle: BlockStore
  ]: F[ApiErr[String]] =
    toposortDag[F, String](maybeDepth = None) {
      case (_, topoSort) =>
        val fetchParents: BlockHash => F[List[BlockHash]] = { blockHash =>
          ProtoUtil.getBlock[F](blockHash) map (_.header.parentsHashList)
        }

        MachineVerifiableDag[F](topoSort, fetchParents)
          .map(_.map(edges => edges.show).mkString("\n"))
          .map(_.asRight[Error])
    }

  def getBlocks[F[_]: Sync: EngineCell: Log: SafetyOracle: BlockStore](
      depth: Option[Int]
  ): F[ApiErr[List[LightBlockInfo]]] =
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
  ): F[ApiErr[LightBlockInfo]] =
    EngineCell[F].read >>= (
      _.withCasper[ApiErr[LightBlockInfo]](
        implicit casper =>
          for {
            dag            <- casper.blockDag
            maybeBlockHash <- dag.lookupByDeployId(id)
            maybeBlock     <- maybeBlockHash.traverse(ProtoUtil.getBlock[F])
            response       <- maybeBlock.traverse(getLightBlockInfo[F])
          } yield response.fold(
            s"Couldn't find block containing deploy with id: ${PrettyPrinter
              .buildStringNoLimit(id)}".asLeft[LightBlockInfo]
          )(
            _.asRight
          ),
        Log[F]
          .warn("Could not find block with deploy, casper instance was not available yet.")
          .as(s"Error: errorMessage".asLeft)
      )
    )

  def getBlock[F[_]: Monad: EngineCell: Log: SafetyOracle: BlockStore: Span](
      hash: String
  ): F[ApiErr[BlockInfo]] = Span[F].trace(GetBlockSource) {

    val errorMessage =
      "Could not get block, casper instance was not available yet."

    def casperResponse(
        implicit casper: MultiParentCasper[F]
    ): F[ApiErr[BlockInfo]] =
      for {
        dag        <- MultiParentCasper[F].blockDag
        maybeBlock <- getBlock[F](hash, dag)
        blockInfo <- maybeBlock match {
                      case Some(block) =>
                        getFullBlockInfo[F](block).map(_.asRight[Error])
                      case None =>
                        s"Error: Failure to find block with hash $hash"
                          .asLeft[BlockInfo]
                          .pure[F]
                    }
      } yield blockInfo

    EngineCell[F].read >>= (_.withCasper[ApiErr[BlockInfo]](
      casperResponse(_),
      Log[F].warn(errorMessage).as(s"Error: $errorMessage".asLeft)
    ))
  }

  private def getBlockInfo[A, F[_]: Monad: SafetyOracle: BlockStore](
      block: BlockMessage,
      constructor: (
          BlockMessage,
          Float
      ) => F[A]
  )(implicit casper: MultiParentCasper[F]): F[A] =
    for {
      dag <- casper.blockDag
      normalizedFaultTolerance <- SafetyOracle[F]
                                   .normalizedFaultTolerance(dag, block.blockHash) // TODO: Warn about parent block finalization
      initialFault   <- casper.normalizedInitialFault(ProtoUtil.weightMap(block))
      faultTolerance = normalizedFaultTolerance - initialFault
      blockInfo      <- constructor(block, faultTolerance)
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
      faultTolerance: Float
  ): F[BlockInfo] =
    for {
      lightBlockInfo <- constructLightBlockInfo[F](block, faultTolerance)
      deploys        = block.body.deploys.toSeq.map(_.toDeployInfo)
    } yield BlockInfo(
      blockInfo = Some(lightBlockInfo),
      deploys = deploys
    )

  private def constructLightBlockInfo[F[_]: Monad: SafetyOracle: BlockStore](
      block: BlockMessage,
      faultTolerance: Float
  ): F[LightBlockInfo] =
    LightBlockInfo(
      blockHash = PrettyPrinter.buildStringNoLimit(block.blockHash),
      sender = PrettyPrinter.buildStringNoLimit(block.sender),
      seqNum = block.seqNum.toLong,
      sig = PrettyPrinter.buildStringNoLimit(block.sig),
      sigAlgorithm = block.sigAlgorithm,
      shardId = block.shardId,
      extraBytes = block.extraBytes,
      version = block.header.version,
      timestamp = block.header.timestamp,
      headerExtraBytes = block.header.extraBytes,
      parentsHashList = block.header.parentsHashList.map(PrettyPrinter.buildStringNoLimit),
      blockNumber = block.body.state.blockNumber,
      preStateHash = PrettyPrinter.buildStringNoLimit(block.body.state.preStateHash),
      postStateHash = PrettyPrinter.buildStringNoLimit(block.body.state.postStateHash),
      bodyExtraBytes = block.body.extraBytes,
      bonds = block.body.state.bonds.map(ProtoUtil.bondToBondInfo),
      blockSize = block.toProto.serializedSize.toString,
      deployCount = block.body.deploys.length,
      faultTolerance = faultTolerance
    ).pure[F]

  private def getBlock[F[_]: Monad: BlockStore](
      hash: String,
      dag: BlockDagRepresentation[F]
  ): F[Option[BlockMessage]] =
    for {
      findResult <- BlockStore[F].find(h => Base16.encode(h.toByteArray).startsWith(hash))
    } yield findResult.headOption match {
      case Some((_, block)) => Some(block)
      case None             => none[BlockMessage]
    }

  private def addResponse[F[_]: Concurrent](
      status: ValidBlockProcessing,
      block: BlockMessage,
      casper: MultiParentCasper[F],
      printUnmatchedSends: Boolean
  ): F[ApiErr[String]] =
    status
      .map { _ =>
        val hash    = block.blockHash.base16String
        val deploys = block.body.deploys.map(_.deploy)
        val maybeUnmatchedSendsOutputF =
          if (printUnmatchedSends) prettyPrintUnmatchedSends(casper, deploys).map(_.some)
          else none[String].pure[F]
        maybeUnmatchedSendsOutputF >>= (
            maybeOutput =>
              s"Success! Block $hash created and added.${maybeOutput.map("\n" + _).getOrElse("")}"
                .asRight[Error]
                .pure[F]
          )
      }
      .leftMap {
        case _: InvalidBlock =>
          s"Failure! Invalid block: $status".asLeft[String].pure[F]
        case BlockError.BlockException(ex) =>
          s"Error during block processing: $ex".asLeft[String].pure[F]
        case BlockError.Processing =>
          "No action taken since other thread is already processing the block."
            .asLeft[String]
            .pure[F]
      }
      .merge

  private def prettyPrintUnmatchedSends[F[_]: Concurrent](
      casper: MultiParentCasper[F],
      deploys: Seq[Signed[DeployData]]
  ): F[String] =
    casper.getRuntimeManager >>= (
      _.withRuntimeLock(runtime => StoragePrinter.prettyPrintUnmatchedSends(deploys, runtime))
    )

  def previewPrivateNames[F[_]: Monad: Log](
      deployer: ByteString,
      timestamp: Long,
      nameQty: Int
  ): F[ApiErr[Seq[ByteString]]] = {
    val rand                = Tools.unforgeableNameRng(PublicKey(deployer.toByteArray), timestamp)
    val safeQty             = nameQty min 1024
    val ids: Seq[BlockHash] = (0 until safeQty).map(_ => ByteString.copyFrom(rand.next()))
    ids.asRight[String].pure[F]
  }

  def lastFinalizedBlock[F[_]: Monad: EngineCell: SafetyOracle: BlockStore: Log]
      : F[ApiErr[BlockInfo]] = {
    val errorMessage = "Could not get last finalized block, casper instance was not available yet."
    EngineCell[F].read >>= (
      _.withCasper[ApiErr[BlockInfo]](
        implicit casper =>
          for {
            lastFinalizedBlock <- casper.lastFinalizedBlock
            blockInfo          <- getFullBlockInfo[F](lastFinalizedBlock)
          } yield blockInfo.asRight,
        Log[F].warn(errorMessage).as(s"Error: $errorMessage".asLeft)
      )
    )
  }

  def isFinalized[F[_]: Monad: EngineCell: SafetyOracle: BlockStore: Log](
      hash: String
  ): F[ApiErr[Boolean]] = {
    val errorMessage =
      "Could not check if block is finalized, casper instance was not available yet."
    EngineCell[F].read >>= (
      _.withCasper[ApiErr[Boolean]](
        implicit casper =>
          for {
            lastFinalizedBlock <- casper.lastFinalizedBlock
            lastFinalizedBlockMetadata = BlockMetadata
              .fromBlock(lastFinalizedBlock, invalid = false)
            dag                   <- casper.blockDag
            givenBlockHash        = ProtoUtil.stringToByteString(hash)
            givenBlockMetadataOpt <- dag.lookup(givenBlockHash)
            result <- givenBlockMetadataOpt match {
                       case None =>
                         s"Could not find block with hash $hash"
                           .asLeft[Boolean]
                           .pure[F]
                       case Some(givenBlockMetadata) =>
                         DagOperations
                           .isDescendantOf(lastFinalizedBlockMetadata, givenBlockMetadata, dag)
                           .map(_.asRight[Error])
                     }
          } yield result,
        Log[F].warn(errorMessage).as(s"Error: $errorMessage".asLeft)
      )
    )
  }

  def bondStatus[F[_]: Monad: EngineCell: Log](
      publicKey: ByteString
  ): F[ApiErr[Boolean]] = {
    val errorMessage =
      "Could not check if validator is bonded, casper instance was not available yet."
    EngineCell[F].read >>= (
      _.withCasper[ApiErr[Boolean]](
        implicit casper =>
          for {
            lastFinalizedBlock <- casper.lastFinalizedBlock
            runtimeManager     <- casper.getRuntimeManager
            postStateHash      = ProtoUtil.postStateHash(lastFinalizedBlock)
            bonds              <- runtimeManager.computeBonds(postStateHash)
            validatorBondOpt   = bonds.find(_.validator == publicKey)
          } yield validatorBondOpt.isDefined.asRight[Error],
        Log[F].warn(errorMessage).as(s"Error: $errorMessage".asLeft)
      )
    )
  }
}
