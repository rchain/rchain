package coop.rchain.casper.api

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.{EventConverter, ProtoUtil}
import coop.rchain.casper._
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Channel
import coop.rchain.rspace.StableHashProvider
import coop.rchain.rspace.trace.{Consume, Produce}
import coop.rchain.shared.Log
import coop.rchain.models.serialization.implicits.serializeChannel
import coop.rchain.rholang.interpreter.{PrettyPrinter => RholangPrettyPrinter}
import scodec.Codec

import scala.collection.immutable

object BlockAPI {

  def deploy[F[_]: Monad: MultiParentCasperConstructor: Log](
      d: DeployData): F[DeployServiceResponse] = {
    def casperDeploy(implicit casper: MultiParentCasper[F]): F[DeployServiceResponse] =
      InterpreterUtil.mkTerm(d.term) match {
        case Right(term) =>
          val deploy = Deploy(
            term = Some(term),
            raw = Some(d)
          )
          for {
            _ <- MultiParentCasper[F].deploy(deploy)
          } yield DeployServiceResponse(success = true, "Success!")

        case Left(err) =>
          DeployServiceResponse(success = false, s"Error in parsing term: \n$err").pure[F]
      }

    MultiParentCasperConstructor
      .withCasper[F, DeployServiceResponse](
        casperDeploy(_),
        DeployServiceResponse(success = false, s"Error: Casper instance not available"))
  }

  def addBlock[F[_]: Monad: MultiParentCasperConstructor: Log](
      b: BlockMessage): F[DeployServiceResponse] =
    MultiParentCasperConstructor.withCasper[F, DeployServiceResponse](
      casper =>
        for {
          status <- casper.addBlock(b)
        } yield addResponse(status.some, b.some),
      DeployServiceResponse(success = false, "Error: Casper instance not available")
    )

  def createBlock[F[_]: Monad: MultiParentCasperConstructor: Log]: F[DeployServiceResponse] =
    MultiParentCasperConstructor.withCasper[F, DeployServiceResponse](
      casper =>
        for {
          maybeBlock <- casper.createBlock
          status     <- maybeBlock.traverse(casper.addBlock)
        } yield addResponse(status, maybeBlock),
      DeployServiceResponse(success = false, "Error: Casper instance not available")
    )

  def getListeningNameResponse[
      F[_]: Monad: MultiParentCasperConstructor: Log: SafetyOracle: BlockStore](
      listeningName: Channel): F[ListeningNameResponse] = {
    def casperResponse(implicit casper: MultiParentCasper[F], channelCodec: Codec[Channel]) =
      for {
        estimates           <- casper.estimator
        tip                 = estimates.head
        internalMap         <- BlockStore[F].asMap()
        mainChain           = ProtoUtil.getMainChain(internalMap, tip, IndexedSeq.empty[BlockMessage])
        maybeRuntimeManager <- casper.getRuntimeManager
        runtimeManager      = maybeRuntimeManager.get // This is safe. Please reluctantly accept until runtimeManager is no longer exposed.
        maybeBlocksWithActiveName <- mainChain.toList.traverse { block =>
                                      val serializedLog =
                                        block.body.fold(Seq.empty[Event])(_.commReductions)
                                      val log =
                                        serializedLog.map(EventConverter.toRspaceEvent).toList
                                      val listeningNameReduced = log.exists {
                                        case Produce(channelHash, _) =>
                                          channelHash == StableHashProvider.hash(
                                            immutable.Seq(listeningName))
                                        case Consume(channelHash, _) =>
                                          channelHash == StableHashProvider.hash(
                                            immutable.Seq(listeningName))
                                        case _ => false
                                      }
                                      if (listeningNameReduced) {
                                        val stateHash =
                                          ProtoUtil.tuplespace(block).get
                                        val data =
                                          runtimeManager.getData(stateHash, listeningName)
                                        for {
                                          blockInfo <- getBlockInfoWithoutTuplespace[F](block)
                                        } yield
                                          Option[DataWithBlockInfo](
                                            DataWithBlockInfo(data, Some(blockInfo)))
                                      } else {
                                        none[DataWithBlockInfo].pure[F]
                                      }
                                    }
        blocksWithActiveName = maybeBlocksWithActiveName.flatten
      } yield
        ListeningNameResponse(status = "Success",
                              blockResults = blocksWithActiveName,
                              length = blocksWithActiveName.length.toLong)

    implicit val channelCodec: Codec[Channel] = serializeChannel.toCodec
    MultiParentCasperConstructor.withCasper[F, ListeningNameResponse](
      casperResponse(_, channelCodec),
      ListeningNameResponse(status = "Error: Casper instance not available"))
  }

  def getBlocksResponse[F[_]: Monad: MultiParentCasperConstructor: Log: SafetyOracle: BlockStore]
    : F[BlocksResponse] = {
    def casperResponse(implicit casper: MultiParentCasper[F]) =
      for {
        estimates   <- MultiParentCasper[F].estimator
        tip         = estimates.head
        internalMap <- BlockStore[F].asMap()
        mainChain: IndexedSeq[BlockMessage] = ProtoUtil.getMainChain(internalMap,
                                                                     tip,
                                                                     IndexedSeq.empty[BlockMessage])
        blockInfos <- mainChain.toList.traverse(getFullBlockInfo[F])
      } yield
        BlocksResponse(status = "Success", blocks = blockInfos, length = blockInfos.length.toLong)

    MultiParentCasperConstructor.withCasper[F, BlocksResponse](
      casperResponse(_),
      BlocksResponse(status = "Error: Casper instance not available"))
  }

  def getBlockQueryResponse[
      F[_]: Monad: MultiParentCasperConstructor: Log: SafetyOracle: BlockStore](
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

    MultiParentCasperConstructor.withCasper[F, BlockQueryResponse](
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
      sender = PrettyPrinter.buildStringNoLimit(block.sender)
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
    BlockStore[F].asMap().map { internalMap: Map[BlockHash, BlockMessage] =>
      val fullHash = internalMap.keys
        .find(h => {
          Base16.encode(h.toByteArray).startsWith(q.hash)
        })
      fullHash.map(h => internalMap(h))
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
    case None => DeployServiceResponse(success = false, "No block was created.")
  }
}
