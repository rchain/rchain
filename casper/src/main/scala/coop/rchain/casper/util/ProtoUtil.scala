package coop.rchain.casper.util

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.{ByteString, Int64Value, StringValue}
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

  def getParentsMetadata[F[_]: Sync: BlockDagStorage](
      b: BlockMetadata,
      dag: DagRepresentation
  ): F[List[BlockMetadata]] =
    // TODO: filter invalid blocks
    b.justifications.traverse(dag.lookupUnsafe(_))

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

  def unsignedBlockProto(
      version: Int,
      sender: PublicKey,
      body: Body,
      justifications: List[BlockHash],
      shardId: String,
      seqNum: Long
  ): BlockMessage = {
    val block = BlockMessage(
      version,
      blockHash = ByteString.EMPTY,
      sender = sender.bytes.toByteString,
      seqNum = seqNum,
      body,
      justifications,
      sig = ByteString.EMPTY,
      sigAlgorithm = "",
      shardId
    )

    val hash = hashBlock(block)

    block.copy(blockHash = hash)
  }

  def hashBlock(blockMessage: BlockMessage): BlockHash =
    ProtoUtil.hashByteArrays(
      blockMessage.body.toProto.toByteArray,
      blockMessage.sender.toByteArray,
      StringValue.of(blockMessage.sigAlgorithm).toByteArray,
      Int64Value.of(blockMessage.seqNum).toByteArray,
      StringValue.of(blockMessage.shardId).toByteArray
    )

  def dependenciesHashesOf(b: BlockMessage): Set[BlockHash] = b.justifications.toSet
}
