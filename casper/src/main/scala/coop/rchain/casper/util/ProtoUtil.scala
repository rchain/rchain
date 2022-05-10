package coop.rchain.casper.util

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.{ByteString, Int32Value, StringValue}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagRepresentation}
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol._
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models._
import coop.rchain.models.syntax._

object ProtoUtil {

  def parentHashes(b: BlockMessage): List[ByteString] =
    b.header.parentsHashList

  def getParents[F[_]: Sync: BlockStore](b: BlockMessage): F[List[BlockMessage]] =
    parentHashes(b).traverse(BlockStore[F].getUnsafe)

  def getParentsMetadata[F[_]: Sync: BlockDagStorage](
      b: BlockMetadata,
      dag: DagRepresentation
  ): F[List[BlockMetadata]] = {
    import cats.instances.list._
    b.parents.traverse(dag.lookupUnsafe(_))
  }

  def getParentMetadatasAboveBlockNumber[F[_]: Sync: BlockDagStorage](
      b: BlockMetadata,
      blockNumber: Long,
      dag: DagRepresentation
  ): F[List[BlockMetadata]] =
    getParentsMetadata(b, dag)
      .map(parents => parents.filter(p => p.blockNum >= blockNumber))

  def deploys(b: BlockMessage): Seq[ProcessedDeploy] =
    b.body.deploys

  def systemDeploys(b: BlockMessage): Seq[ProcessedSystemDeploy] =
    b.body.systemDeploys

  def postStateHash(b: BlockMessage): ByteString =
    b.body.state.postStateHash

  def preStateHash(b: BlockMessage): ByteString =
    b.body.state.preStateHash

  def bonds(b: BlockMessage): Seq[Bond] =
    b.body.state.bonds

  def blockNumber(b: BlockMessage): Long =
    b.body.state.blockNumber

  def bondToBondInfo(bond: Bond): BondInfo =
    BondInfo(validator = PrettyPrinter.buildStringNoLimit(bond.validator), stake = bond.stake)

  def maxBlockNumberMetadata(blocks: Seq[BlockMetadata]): Long = blocks.foldLeft(-1L) {
    case (acc, b) => math.max(acc, b.blockNum)
  }

  def justificationsToJustificationInfos(justification: Justification) =
    JustificationInfo(
      PrettyPrinter.buildStringNoLimit(justification.validator),
      PrettyPrinter.buildStringNoLimit(justification.latestBlockHash)
    )

  def toJustification(
      latestMessages: collection.Map[Validator, BlockMetadata]
  ): Seq[Justification] =
    latestMessages.toSeq.map {
      case (validator, blockMetadata) => Justification(validator, blockMetadata.blockHash)
    }

  def hashByteArrays(items: Array[Byte]*): ByteString =
    ByteString.copyFrom(Blake2b256.hash(Array.concat(items: _*)))

  def blockHeader(
      parentHashes: Seq[ByteString],
      version: Long,
      timestamp: Long
  ): Header =
    Header(
      parentHashes.toList,
      timestamp,
      version
    )

  def unsignedBlockProto(
      sender: PublicKey,
      body: Body,
      header: Header,
      justifications: List[BlockHash],
      shardId: String,
      seqNum: Int = 0
  ): BlockMessage = {
    val block = BlockMessage(
      blockHash = ByteString.EMPTY,
      header,
      body,
      justifications,
      sender = sender.bytes.toByteString,
      seqNum = seqNum,
      sig = ByteString.EMPTY,
      sigAlgorithm = "",
      shardId,
      extraBytes = ByteString.EMPTY
    )

    val hash = hashBlock(block)

    block.copy(blockHash = hash)
  }

  def hashBlock(blockMessage: BlockMessage): BlockHash =
    ProtoUtil.hashByteArrays(
      blockMessage.header.toProto.toByteArray,
      blockMessage.body.toProto.toByteArray,
      blockMessage.sender.toByteArray,
      StringValue.of(blockMessage.sigAlgorithm).toByteArray,
      Int32Value.of(blockMessage.seqNum).toByteArray,
      StringValue.of(blockMessage.shardId).toByteArray,
      blockMessage.extraBytes.toByteArray
    )

  def dependenciesHashesOf(b: BlockMessage): Set[BlockHash] = {
    val missingParents        = parentHashes(b).toSet
    val missingJustifications = b.justifications.toSet
    missingParents union missingJustifications
  }
}
