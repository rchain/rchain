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
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.dag.DagOps
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models._

import scala.collection.immutable

object ProtoUtil {

  def creatorJustification(block: BlockMessage): Option[Justification] =
    block.justifications.find(_.validator == block.sender)

  def creatorJustification(block: BlockMetadata): Option[Justification] =
    block.justifications.find(_.validator == block.sender)

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

  def toLatestMessageHashes(
      justifications: Seq[Justification]
  ): immutable.Map[Validator, BlockHash] =
    justifications.foldLeft(Map.empty[Validator, BlockHash]) {
      case (acc, Justification(validator, block)) =>
        acc.updated(validator, block)
    }

  def toLatestMessage[F[_]: Sync: BlockDagStorage](
      justifications: Seq[Justification],
      dag: DagRepresentation
  ): F[immutable.Map[Validator, BlockMetadata]] =
    justifications.toList.foldM(Map.empty[Validator, BlockMetadata]) {
      case (acc, Justification(validator, hash)) =>
        for {
          blockMetadataOpt <- dag.lookup(hash)
          blockMetadata <- blockMetadataOpt
                            .map(_.pure[F])
                            .getOrElse(
                              Sync[F].raiseError(
                                new RuntimeException(
                                  s"Could not find a block for ${PrettyPrinter.buildString(hash)} in the DAG storage"
                                )
                              )
                            )
        } yield acc.updated(validator, blockMetadata)
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
      body: Body,
      header: Header,
      justifications: Seq[Justification],
      shardId: String,
      seqNum: Int = 0
  ): BlockMessage = {
    val block = BlockMessage(
      blockHash = ByteString.EMPTY,
      header,
      body,
      justifications.toList,
      sender = ByteString.EMPTY,
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

  def dependenciesHashesOf(b: BlockMessage): List[BlockHash] = {
    val missingParents = parentHashes(b).toSet
    val missingJustifications = b.justifications
      .map(_.latestBlockHash)
      .toSet
    (missingParents union missingJustifications).toList
  }

  // Return hashes of all blocks that are yet to be seen by the passed in block
  def unseenBlockHashes[F[_]: Sync: BlockDagStorage](
      dag: DagRepresentation,
      block: BlockMessage
  ): F[Set[BlockHash]] =
    for {
      dagsLatestMessages   <- dag.latestMessages
      blocksLatestMessages <- toLatestMessage(block.justifications, dag)

      // From input block perspective we want to find what latest messages are not seen
      //  that are in the DAG latest messages.
      // - if validator is not in the justification of the block
      // - if justification contains validator's newer latest message
      unseenLatestMessages = dagsLatestMessages.filter {
        case (validator, dagLatestMessage) =>
          val validatorInJustification = blocksLatestMessages.contains(validator)
          def blockHasNewerLatestMessage =
            blocksLatestMessages.get(validator).map(dagLatestMessage.seqNum > _.seqNum)

          !validatorInJustification || (validatorInJustification && blockHasNewerLatestMessage.get)
      }

      unseenBlockHashes <- unseenLatestMessages.toStream
                            .traverse {
                              case (validator, unseenLatestMessage) =>
                                getCreatorBlocksBetween(
                                  dag,
                                  unseenLatestMessage,
                                  blocksLatestMessages.get(validator)
                                )
                            }
                            .map(_.flatten.toSet)
    } yield unseenBlockHashes -- blocksLatestMessages.values.map(_.blockHash) - block.blockHash

  private def getCreatorBlocksBetween[F[_]: Sync: BlockDagStorage](
      dag: DagRepresentation,
      topBlock: BlockMetadata,
      bottomBlock: Option[BlockMetadata]
  ): F[Set[BlockHash]] =
    bottomBlock match {
      case None => Set(topBlock.blockHash).pure[F]
      case Some(bottomBlock) =>
        DagOps
          .bfTraverseF(List(topBlock))(
            nextCreatorBlock =>
              getCreatorJustificationUnlessGoal(
                dag,
                nextCreatorBlock,
                bottomBlock
              )
          )
          .map(_.blockHash)
          .toSet
    }

  private def getCreatorJustificationUnlessGoal[F[_]: Sync: BlockDagStorage](
      dag: DagRepresentation,
      block: BlockMetadata,
      goal: BlockMetadata
  ): F[List[BlockMetadata]] =
    creatorJustification(block) match {
      case Some(Justification(_, hash)) =>
        dag.lookup(hash).flatMap {
          case Some(creatorJustification) =>
            if (creatorJustification == goal) {
              List.empty[BlockMetadata].pure
            } else {
              List(creatorJustification).pure
            }
          case None =>
            Sync[F].raiseError[List[BlockMetadata]](
              new RuntimeException(
                s"BlockDAG is missing justification ${PrettyPrinter
                  .buildString(hash)} for ${PrettyPrinter.buildString(block.blockHash)}."
              )
            )
        }
      case None =>
        List.empty[BlockMetadata].pure
    }
}
