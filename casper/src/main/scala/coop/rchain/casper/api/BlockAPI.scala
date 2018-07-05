package coop.rchain.casper.api

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import com.google.protobuf.empty.Empty
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.{
  BlockDag,
  MultiParentCasper,
  MultiParentCasperConstructor,
  PrettyPrinter,
  SafetyOracle
}
import coop.rchain.crypto.codec.Base16
import coop.rchain.shared.Log

import scala.annotation.tailrec

object BlockAPI {
  def createBlock[F[_]: Monad: MultiParentCasperConstructor: Log]: F[MaybeBlockMessage] =
    MultiParentCasperConstructor.withCasper[F, MaybeBlockMessage](
      _.createBlock.map(MaybeBlockMessage.apply),
      MaybeBlockMessage.defaultInstance)

  def addBlock[F[_]: Monad: MultiParentCasperConstructor: Log](b: BlockMessage): F[Empty] =
    MultiParentCasperConstructor
      .withCasper[F, Empty](_.addBlock(b).map(_ => Empty.defaultInstance), Empty.defaultInstance)

  def getBlocksResponse[F[_]: Monad: MultiParentCasperConstructor: Log: SafetyOracle]
    : F[BlocksResponse] = {
    def casperResponse(implicit casper: MultiParentCasper[F]) =
      for {
        estimates <- MultiParentCasper[F].estimator
        dag       <- MultiParentCasper[F].blockDag
        tip       = estimates.head
        mainChain: IndexedSeq[BlockMessage] = ProtoUtil.getMainChain(dag,
                                                                     tip,
                                                                     IndexedSeq.empty[BlockMessage])
        blockInfos <- mainChain.toList.traverse(getBlockInfo[F])
      } yield
        BlocksResponse(status = "Success", blocks = blockInfos, length = blockInfos.length.toLong)

    MultiParentCasperConstructor.withCasper[F, BlocksResponse](
      casperResponse(_),
      BlocksResponse(status = "Error: Casper instance not available"))
  }

  def getBlockQueryResponse[F[_]: Monad: MultiParentCasperConstructor: Log: SafetyOracle](
      q: BlockQuery): F[BlockQueryResponse] = {
    def casperResponse(implicit casper: MultiParentCasper[F]) =
      for {
        dag        <- MultiParentCasper[F].blockDag
        maybeBlock = getBlock[F](q, dag)
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

  private def getBlockInfo[F[_]: Monad: MultiParentCasper: SafetyOracle](
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
        parentsHashList = parentsHashList.map(PrettyPrinter.buildStringNoLimit)
      )
    }

  private def getBlock[F[_]: Monad: MultiParentCasper](q: BlockQuery,
                                                       dag: BlockDag): Option[BlockMessage] = {
    val fullHash = dag.blockLookup.keys.find(h => {
      Base16.encode(h.toByteArray).startsWith(q.hash)
    })
    fullHash.map(h => dag.blockLookup(h))
  }
}
