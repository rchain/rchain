package coop.rchain.casper.api

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper._
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.crypto.codec.Base16
import coop.rchain.shared.Log

object BlockAPI {

  def deploy[F[_]: Monad: MultiParentCasperConstructor: Log](
      d: DeployString): F[DeployServiceResponse] = {
    def casperDeploy(implicit casper: MultiParentCasper[F]): F[DeployServiceResponse] =
      InterpreterUtil.mkTerm(d.term) match {
        case Right(term) =>
          val deploy = Deploy(
            term = Some(term),
            raw = Some(d)
          )
          for {
            _ <- MultiParentCasper[F].deploy(deploy)
          } yield DeployServiceResponse(true, "Success!")

        case Left(err) =>
          DeployServiceResponse(false, s"Error in parsing term: \n$err").pure[F]
      }

    MultiParentCasperConstructor
      .withCasper[F, DeployServiceResponse](
        casperDeploy(_),
        DeployServiceResponse(false, s"Error: Casper instance not available"))
  }

  def createBlock[F[_]: Monad: MultiParentCasperConstructor: Log]: F[MaybeBlockMessage] =
    MultiParentCasperConstructor.withCasper[F, MaybeBlockMessage](
      _.createBlock.map(MaybeBlockMessage.apply),
      MaybeBlockMessage.defaultInstance)

  def addBlock[F[_]: Monad: MultiParentCasperConstructor: Log](
      b: BlockMessage): F[DeployServiceResponse] = {
    def doAddBlock(implicit casper: MultiParentCasper[F]): F[DeployServiceResponse] =
      for {
        status <- MultiParentCasper[F].addBlock(b)
      } yield
        status match {
          case _: InvalidBlock => DeployServiceResponse(false, s"Failure! Invalid block: $status")
          case _: ValidBlock   => DeployServiceResponse(true, s"Success! $status")
          case BlockException(ex) =>
            DeployServiceResponse(false, s"Error during block processing: $ex")
          case Processing =>
            DeployServiceResponse(
              false,
              s"No action taken since another thread is already processing the block.")
        }

    MultiParentCasperConstructor
      .withCasper[F, DeployServiceResponse](
        doAddBlock(_),
        DeployServiceResponse(false, "Error: Casper instance not available"))
  }

  def getBlocksResponse[F[_]: Monad: MultiParentCasperConstructor: Log: SafetyOracle: BlockStore]
    : F[BlocksResponse] = {
    def casperResponse(implicit casper: MultiParentCasper[F]) =
      for {
        estimates   <- MultiParentCasper[F].estimator
        dag         <- MultiParentCasper[F].blockDag
        tip         = estimates.head
        internalMap <- BlockStore[F].asMap()
        mainChain: IndexedSeq[BlockMessage] = ProtoUtil.getMainChain(internalMap,
                                                                     tip,
                                                                     IndexedSeq.empty[BlockMessage])
        blockInfos <- mainChain.toList.traverse(getBlockInfo[F])
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
                               case Some(block) => {
                                 for {
                                   blockInfo <- getBlockInfo[F](block)
                                 } yield
                                   BlockQueryResponse(status = "Success",
                                                      blockInfo = Some(blockInfo))
                               }
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

  private def getBlockInfo[F[_]: Monad: MultiParentCasper: SafetyOracle: BlockStore](
      block: BlockMessage): F[BlockInfo] =
    for {
      header      <- block.header.getOrElse(Header.defaultInstance).pure[F]
      version     <- header.version.pure[F]
      deployCount <- header.deployCount.pure[F]
      tsHash <- {
        val ps = block.body.flatMap(_.postState)
        ps.fold(ByteString.EMPTY)(_.tuplespace).pure[F]
      }
      tsDesc                   <- MultiParentCasper[F].storageContents(tsHash)
      timestamp                <- header.timestamp.pure[F]
      mainParent               <- header.parentsHashList.headOption.getOrElse(ByteString.EMPTY).pure[F]
      parentsHashList          <- header.parentsHashList.pure[F]
      dag                      <- MultiParentCasper[F].blockDag
      normalizedFaultTolerance <- SafetyOracle[F].normalizedFaultTolerance(dag, block)
      initialFault             <- MultiParentCasper[F].normalizedInitialFault(ProtoUtil.weightMap(block))
    } yield {
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
    }

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
}
