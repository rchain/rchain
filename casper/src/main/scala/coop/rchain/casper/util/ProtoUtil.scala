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
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models._
import coop.rchain.models.syntax._

object ProtoUtil {

  def getParentsMetadata[F[_]: Sync: BlockDagStorage](b: BlockMetadata): F[List[BlockMetadata]] =
    b.justifications
      .traverse(BlockDagStorage[F].lookupUnsafe(_))
      .map(_.filter(!_.invalid))

  def getParentMetadatasAboveBlockNumber[F[_]: Sync: BlockDagStorage](
      b: BlockMetadata,
      blockNumber: Long
  ): F[List[BlockMetadata]] =
    getParentsMetadata(b).map(parents => parents.filter(p => p.blockNum >= blockNumber))

  def deploys(b: BlockMessage): Seq[ProcessedDeploy] =
    b.state.deploys

  def systemDeploys(b: BlockMessage): Seq[ProcessedSystemDeploy] =
    b.state.systemDeploys

  def bondToBondInfo(bond: (Validator, Long)): BondInfo =
    BondInfo(validator = PrettyPrinter.buildStringNoLimit(bond._1), stake = bond._2)

  def maxBlockNumberMetadata(blocks: Seq[BlockMetadata]): Long = blocks.foldLeft(-1L) {
    case (acc, b) => math.max(acc, b.blockNum)
  }

  def unsignedBlockProto(
      version: Int,
      shardId: String,
      blockNumber: Long,
      sender: PublicKey,
      seqNum: Long,
      preStateHash: ByteString,
      postStateHash: ByteString,
      justifications: List[BlockHash],
      bonds: Map[Validator, Long],
      rejectedDeploys: List[ByteString],
      state: RholangState
  ): BlockMessage = {
    val block = BlockMessage(
      version,
      shardId,
      blockHash = ByteString.EMPTY,
      blockNumber = blockNumber,
      sender = sender.bytes.toByteString,
      seqNum = seqNum,
      preStateHash = preStateHash,
      postStateHash = postStateHash,
      justifications,
      bonds,
      rejectedDeploys,
      state,
      // Signature algorithm is now part of the block hash
      //  so it should be set immediately.
      // [TG] I couldn't find a reason why is part of block hash.
      sigAlgorithm = Secp256k1.name,
      sig = ByteString.EMPTY
    )

    val hash = hashBlock(block)

    block.copy(blockHash = hash)
  }

  /**
    * Create hash of a BlockMessage, all fields must be included except signature
    */
  def hashBlock(blockMessage: BlockMessage): BlockHash = {
    val emptyBytes = ByteString.EMPTY
    val blockClearSigData = blockMessage.copy(
      blockHash = emptyBytes,
      sig = emptyBytes
    )
    Blake2b256.hash(blockClearSigData.toProto.toByteArray).toByteString
  }

  def dependenciesHashesOf(b: BlockMessage): Set[BlockHash] = b.justifications.toSet
}
