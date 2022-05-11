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

  def bondToBondInfo(bond: (Validator, Long)): BondInfo =
    BondInfo(validator = PrettyPrinter.buildStringNoLimit(bond._1), stake = bond._2)

  def maxBlockNumberMetadata(blocks: Seq[BlockMetadata]): Long = blocks.foldLeft(-1L) {
    case (acc, b) => math.max(acc, b.blockNum)
  }

  def unsignedBlockProto(
      version: Int,
      blockNumber: Long,
      sender: PublicKey,
      preStateHash: ByteString,
      postStateHash: ByteString,
      body: Body,
      justifications: List[BlockHash],
      bonds: Map[Validator, Long],
      shardId: String,
      seqNum: Long
  ): BlockMessage = {
    val block = BlockMessage(
      version,
      blockHash = ByteString.EMPTY,
      blockNumber = blockNumber,
      sender = sender.bytes.toByteString,
      seqNum = seqNum,
      preStateHash = preStateHash,
      postStateHash = postStateHash,
      body,
      justifications,
      bonds,
      sig = ByteString.EMPTY,
      sigAlgorithm = "",
      shardId
    )

    val hash = hashBlock(block)

    block.copy(blockHash = hash)
  }

  /**
    * Create hash of a BlockMessage, all fields must be included except signature
    */
  def hashBlock(blockMessage: BlockMessage): BlockHash = {
    assert(blockMessage.sig.isEmpty, {
      val blockStr = PrettyPrinter.buildString(blockMessage)
      s"Signature must be empty to hash a BlockMessage $blockStr"
    })
    Blake2b256.hash(blockMessage.toProto.toByteArray).toByteString
  }

  def dependenciesHashesOf(b: BlockMessage): Set[BlockHash] = b.justifications.toSet
}
