package coop.rchain.casper.api

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper._
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.{BindPattern, Channel, Par}
import coop.rchain.models.rholang.sort.Sortable
import coop.rchain.rspace.StableHashProvider
import coop.rchain.rspace.trace.{COMM, Consume, Produce}
import coop.rchain.shared.Log
import coop.rchain.models.serialization.implicits.serializeChannel
import coop.rchain.rholang.interpreter.{PrettyPrinter => RholangPrettyPrinter}
import coop.rchain.models.rholang.sort.Sortable._
import scodec.Codec

import scala.collection.immutable
import coop.rchain.catscontrib._
import coop.rchain.casper.util.{EventConverter, ProtoUtil}
import coop.rchain.casper._
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil

object BlockAPI {

  def deploy[F[_]: Monad: MultiParentCasperRef: Log](d: DeployData): F[DeployServiceResponse] = {
    def casperDeploy(implicit casper: MultiParentCasper[F]): F[DeployServiceResponse] =
      for {
        r <- MultiParentCasper[F].deploy(d)
        re <- r match {
               case Right(_)  => DeployServiceResponse(success = true, "Success!").pure[F]
               case Left(err) => DeployServiceResponse(success = false, err.getMessage).pure[F]
             }
      } yield re

    MultiParentCasperRef
      .withCasper[F, DeployServiceResponse](
        casperDeploy(_),
        DeployServiceResponse(success = false, s"Error: Casper instance not available"))
  }

  def addBlock[F[_]: Monad: MultiParentCasperRef: Log](b: BlockMessage): F[DeployServiceResponse] =
    MultiParentCasperRef.withCasper[F, DeployServiceResponse](
      casper =>
        for {
          status <- casper.addBlock(b)
        } yield addResponse(status.some, b.some),
      DeployServiceResponse(success = false, "Error: Casper instance not available")
    )

  def createBlock[F[_]: Monad: MultiParentCasperRef: Log]: F[DeployServiceResponse] =
    MultiParentCasperRef.withCasper[F, DeployServiceResponse](
      casper =>
        for {
          maybeBlock <- casper.createBlock
          status     <- maybeBlock.traverse(casper.addBlock)
        } yield addResponse(status, maybeBlock),
      DeployServiceResponse(success = false, "Error: Casper instance not available")
    )

  def getListeningNameDataResponse[
      F[_]: Monad: MultiParentCasperRef: Log: SafetyOracle: BlockStore](
      listeningName: Channel): F[ListeningNameDataResponse] = {
    def casperResponse(implicit casper: MultiParentCasper[F], channelCodec: Codec[Channel]) =
      for {
        mainChain           <- getMainChainFromTip[F]
        maybeRuntimeManager <- casper.getRuntimeManager
        runtimeManager      = maybeRuntimeManager.get // This is safe. Please reluctantly accept until runtimeManager is no longer exposed.
        sortedListeningName = channelSortable.sortMatch(listeningName).term
        maybeBlocksWithActiveName <- mainChain.toList.traverse { block =>
                                      getDataWithBlockInfo[F](runtimeManager,
                                                              sortedListeningName,
                                                              block)
                                    }
        blocksWithActiveName = maybeBlocksWithActiveName.flatten
      } yield
        ListeningNameDataResponse(status = "Success",
                                  blockResults = blocksWithActiveName,
                                  length = blocksWithActiveName.length)

    implicit val channelCodec: Codec[Channel] = serializeChannel.toCodec
    MultiParentCasperRef.withCasper[F, ListeningNameDataResponse](
      casperResponse(_, channelCodec),
      ListeningNameDataResponse(status = "Error: Casper instance not available"))
  }

  def getListeningNameContinuationResponse[
      F[_]: Monad: MultiParentCasperRef: Log: SafetyOracle: BlockStore](
      listeningNames: Channels): F[ListeningNameContinuationResponse] = {
    def casperResponse(implicit casper: MultiParentCasper[F], channelCodec: Codec[Channel]) =
      for {
        mainChain           <- getMainChainFromTip[F]
        maybeRuntimeManager <- casper.getRuntimeManager
        runtimeManager      = maybeRuntimeManager.get // This is safe. Please reluctantly accept until runtimeManager is no longer exposed.
        sortedListeningNames = immutable.Seq(
          listeningNames.channels.map(channelSortable.sortMatch(_).term): _*)
        maybeBlocksWithActiveName <- mainChain.toList.traverse { block =>
                                      getContinuationsWithBlockInfo[F](runtimeManager,
                                                                       sortedListeningNames,
                                                                       block)
                                    }
        blocksWithActiveName = maybeBlocksWithActiveName.flatten
      } yield
        ListeningNameContinuationResponse(status = "Success",
                                          blockResults = blocksWithActiveName,
                                          length = blocksWithActiveName.length)

    implicit val channelCodec: Codec[Channel] = serializeChannel.toCodec
    MultiParentCasperRef.withCasper[F, ListeningNameContinuationResponse](
      casperResponse(_, channelCodec),
      ListeningNameContinuationResponse(status = "Error: Casper instance not available"))
  }

  private def getMainChainFromTip[F[_]: Monad: MultiParentCasper: Log: SafetyOracle: BlockStore]
    : F[IndexedSeq[BlockMessage]] =
    for {
      estimates <- MultiParentCasper[F].estimator
      tip       = estimates.head
      mainChain <- ProtoUtil.getMainChain[F](tip, IndexedSeq.empty[BlockMessage])
    } yield mainChain

  private def getDataWithBlockInfo[F[_]: Monad: MultiParentCasper: Log: SafetyOracle: BlockStore](
      runtimeManager: RuntimeManager,
      sortedListeningName: Channel,
      block: BlockMessage)(implicit channelCodec: Codec[Channel]): F[Option[DataWithBlockInfo]] =
    if (isListeningNameReduced(block, immutable.Seq(sortedListeningName))) {
      val stateHash =
        ProtoUtil.tuplespace(block).get
      val data =
        runtimeManager.getData(stateHash, sortedListeningName)
      for {
        blockInfo <- getBlockInfoWithoutTuplespace[F](block)
      } yield Option[DataWithBlockInfo](DataWithBlockInfo(data, Some(blockInfo)))
    } else {
      none[DataWithBlockInfo].pure[F]
    }

  private def getContinuationsWithBlockInfo[
      F[_]: Monad: MultiParentCasper: Log: SafetyOracle: BlockStore](
      runtimeManager: RuntimeManager,
      sortedListeningNames: immutable.Seq[Channel],
      block: BlockMessage)(
      implicit channelCodec: Codec[Channel]): F[Option[ContinuationsWithBlockInfo]] =
    if (isListeningNameReduced(block, sortedListeningNames)) {
      val stateHash =
        ProtoUtil.tuplespace(block).get
      val continuations: Seq[(Seq[BindPattern], Par)] =
        runtimeManager.getContinuation(stateHash, sortedListeningNames)
      val continuationInfos = continuations.map(continuation =>
        WaitingContinuationInfo(continuation._1, Some(continuation._2)))
      for {
        blockInfo <- getBlockInfoWithoutTuplespace[F](block)
      } yield
        Option[ContinuationsWithBlockInfo](
          ContinuationsWithBlockInfo(continuationInfos, Some(blockInfo)))
    } else {
      none[ContinuationsWithBlockInfo].pure[F]
    }

  private def isListeningNameReduced(
      block: BlockMessage,
      sortedListeningName: immutable.Seq[Channel])(implicit channelCodec: Codec[Channel]) = {
    val serializedLog = for {
      bd    <- block.body.toSeq
      pd    <- bd.deploys
      event <- pd.log
    } yield event
    val log =
      serializedLog.map(EventConverter.toRspaceEvent).toList
    log.exists {
      case Produce(channelHash, _) =>
        channelHash == StableHashProvider.hash(sortedListeningName)
      case Consume(channelHash, _) =>
        channelHash == StableHashProvider.hash(sortedListeningName)
      case COMM(consume, produces) =>
        consume.channelsHash == StableHashProvider.hash(sortedListeningName) ||
          produces.exists(produce =>
            produce.channelsHash == StableHashProvider.hash(sortedListeningName))
    }
  }

  def getBlocksResponse[F[_]: Monad: MultiParentCasperRef: Log: SafetyOracle: BlockStore]
    : F[BlocksResponse] = {
    def casperResponse(implicit casper: MultiParentCasper[F]) =
      for {
        estimates  <- MultiParentCasper[F].estimator
        tip        = estimates.head
        mainChain  <- ProtoUtil.getMainChain[F](tip, IndexedSeq.empty[BlockMessage])
        blockInfos <- mainChain.toList.traverse(getFullBlockInfo[F])
      } yield
        BlocksResponse(status = "Success", blocks = blockInfos, length = blockInfos.length.toLong)

    MultiParentCasperRef.withCasper[F, BlocksResponse](
      casperResponse(_),
      BlocksResponse(status = "Error: Casper instance not available"))
  }

  def getBlockQueryResponse[F[_]: Monad: MultiParentCasperRef: Log: SafetyOracle: BlockStore](
      q: BlockQuery): F[BlockQueryResponse] = {
    def casperResponse(implicit casper: MultiParentCasper[F]) =
      for {
        dag        <- MultiParentCasper[F].blockDag
        maybeBlock <- getBlock[F](q, dag)
        blockQueryResponse <- maybeBlock match {
                               case Some(block) =>
                                 for {
                                   blockInfo <- getFullBlockInfo[F](block)
                                 } yield
                                   BlockQueryResponse(status = "Success",
                                                      blockInfo = Some(blockInfo))
                               case None =>
                                 BlockQueryResponse(
                                   status = s"Error: Failure to find block with hash ${q.hash}")
                                   .pure[F]
                             }
      } yield blockQueryResponse

    MultiParentCasperRef.withCasper[F, BlockQueryResponse](
      casperResponse(_),
      BlockQueryResponse(status = "Error: Casper instance not available"))
  }

  private def getBlockInfo[A, F[_]: Monad: MultiParentCasper: SafetyOracle: BlockStore](
      block: BlockMessage,
      constructor: (BlockMessage,
                    Long,
                    Int,
                    BlockHash,
                    String,
                    Long,
                    BlockHash,
                    Seq[BlockHash],
                    Float,
                    Float) => A): F[A] =
    for {
      dag         <- MultiParentCasper[F].blockDag
      header      = block.header.getOrElse(Header.defaultInstance)
      version     = header.version
      deployCount = header.deployCount
      tsHash = ProtoUtil.tuplespace(block) match {
        case Some(hash) => hash
        case None       => ByteString.EMPTY
      }
      tsDesc                   <- MultiParentCasper[F].storageContents(tsHash)
      timestamp                = header.timestamp
      mainParent               = header.parentsHashList.headOption.getOrElse(ByteString.EMPTY)
      parentsHashList          = header.parentsHashList
      normalizedFaultTolerance <- SafetyOracle[F].normalizedFaultTolerance(dag, block)
      initialFault             <- MultiParentCasper[F].normalizedInitialFault(ProtoUtil.weightMap(block))
    } yield
      constructor(block,
                  version,
                  deployCount,
                  tsHash,
                  tsDesc,
                  timestamp,
                  mainParent,
                  parentsHashList,
                  normalizedFaultTolerance,
                  initialFault)

  private def getFullBlockInfo[F[_]: Monad: MultiParentCasper: SafetyOracle: BlockStore](
      block: BlockMessage): F[BlockInfo] = getBlockInfo[BlockInfo, F](block, constructBlockInfo[F])
  private def getBlockInfoWithoutTuplespace[
      F[_]: Monad: MultiParentCasper: SafetyOracle: BlockStore](
      block: BlockMessage): F[BlockInfoWithoutTuplespace] =
    getBlockInfo[BlockInfoWithoutTuplespace, F](block, constructBlockInfoWithoutTuplespace[F])

  private def constructBlockInfo[F[_]: Monad: MultiParentCasper: SafetyOracle: BlockStore](
      block: BlockMessage,
      version: Long,
      deployCount: Int,
      tsHash: BlockHash,
      tsDesc: String,
      timestamp: Long,
      mainParent: BlockHash,
      parentsHashList: Seq[BlockHash],
      normalizedFaultTolerance: Float,
      initialFault: Float): BlockInfo =
    BlockInfo(
      blockHash = PrettyPrinter.buildStringNoLimit(block.blockHash),
      blockSize = block.serializedSize.toString,
      blockNumber = ProtoUtil.blockNumber(block),
      version = version,
      deployCount = deployCount,
      tupleSpaceHash = PrettyPrinter.buildStringNoLimit(tsHash),
      tupleSpaceDump = tsDesc,
      timestamp = timestamp,
      faultTolerance = normalizedFaultTolerance - initialFault,
      mainParentHash = PrettyPrinter.buildStringNoLimit(mainParent),
      parentsHashList = parentsHashList.map(PrettyPrinter.buildStringNoLimit),
      sender = PrettyPrinter.buildStringNoLimit(block.sender),
      shardId = block.shardId
    )
  private def constructBlockInfoWithoutTuplespace[
      F[_]: Monad: MultiParentCasper: SafetyOracle: BlockStore](
      block: BlockMessage,
      version: Long,
      deployCount: Int,
      tsHash: BlockHash,
      tsDesc: String,
      timestamp: Long,
      mainParent: BlockHash,
      parentsHashList: Seq[BlockHash],
      normalizedFaultTolerance: Float,
      initialFault: Float): BlockInfoWithoutTuplespace =
    BlockInfoWithoutTuplespace(
      blockHash = PrettyPrinter.buildStringNoLimit(block.blockHash),
      blockSize = block.serializedSize.toString,
      blockNumber = ProtoUtil.blockNumber(block),
      version = version,
      deployCount = deployCount,
      tupleSpaceHash = PrettyPrinter.buildStringNoLimit(tsHash),
      timestamp = timestamp,
      faultTolerance = normalizedFaultTolerance - initialFault,
      mainParentHash = PrettyPrinter.buildStringNoLimit(mainParent),
      parentsHashList = parentsHashList.map(PrettyPrinter.buildStringNoLimit),
      sender = PrettyPrinter.buildStringNoLimit(block.sender)
    )

  private def getBlock[F[_]: Monad: MultiParentCasper: BlockStore](
      q: BlockQuery,
      dag: BlockDag): F[Option[BlockMessage]] =
    for {
      findResult <- BlockStore[F].find(h => {
                     Base16.encode(h.toByteArray).startsWith(q.hash)
                   })
    } yield
      findResult.headOption match {
        case Some((_, block)) =>
          Some(block)
        case None =>
          none[BlockMessage]
      }

  private def addResponse(status: Option[BlockStatus],
                          maybeBlock: Option[BlockMessage]): DeployServiceResponse = status match {
    case Some(_: InvalidBlock) =>
      DeployServiceResponse(success = false, s"Failure! Invalid block: $status")
    case Some(_: ValidBlock) =>
      val hash = PrettyPrinter.buildString(maybeBlock.get.blockHash)
      DeployServiceResponse(success = true, s"Success! Block $hash created and added.")
    case Some(BlockException(ex)) =>
      DeployServiceResponse(success = false, s"Error during block processing: $ex")
    case Some(Processing) =>
      DeployServiceResponse(success = false,
                            "No action taken since other thread is already processing the block.")
    case None =>
      DeployServiceResponse(
        success = false,
        "No block was created. Either no new deploys have been received or the node may be in read-only mode.")
  }
}
