package coop.rchain.casper.util

import cats.Monad
import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.{ByteString, Int32Value, StringValue}
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.{DeployData, _}
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Signed
import coop.rchain.dag.DagOps
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models._
import coop.rchain.rholang.interpreter.DeployParameters
import coop.rchain.shared.Base16
import coop.rchain.shared.syntax._

import java.nio.charset.StandardCharsets
import scala.collection.immutable
import scala.collection.immutable.Map

object ProtoUtil {
  def getMainChainUntilDepth[F[_]: Sync: BlockStore](
      estimate: BlockMessage,
      acc: IndexedSeq[BlockMessage],
      depth: Int
  ): F[IndexedSeq[BlockMessage]] = {
    val parentsHashes       = parentHashes(estimate)
    val maybeMainParentHash = parentsHashes.headOption
    for {
      mainChain <- maybeMainParentHash match {
                    case Some(mainParentHash) =>
                      for {
                        updatedEstimate <- BlockStore[F].getUnsafe(mainParentHash)
                        depthDelta      = blockNumber(updatedEstimate) - blockNumber(estimate)
                        newDepth        = depth + depthDelta.toInt
                        mainChain <- if (newDepth <= 0) {
                                      (acc :+ estimate).pure
                                    } else {
                                      getMainChainUntilDepth(
                                        updatedEstimate,
                                        acc :+ estimate,
                                        newDepth
                                      )
                                    }
                      } yield mainChain
                    case None => (acc :+ estimate).pure
                  }
    } yield mainChain
  }

  def creatorJustification(block: BlockMessage): Option[Justification] =
    block.justifications.find(_.validator == block.sender)

  def creatorJustification(block: BlockMetadata): Option[Justification] =
    block.justifications.find(_.validator == block.sender)

  /**
    * Since the creator justification is unique
    * we don't need to return a list. However, the bfTraverseF
    * requires a list to be returned. When we reach the goalFunc,
    * we return an empty list.
    */
  def getCreatorJustificationAsListUntilGoalInMemory[F[_]: Monad](
      blockDag: BlockDagRepresentation[F],
      blockHash: BlockHash,
      goalFunc: BlockHash => Boolean = _ => false
  ): F[List[BlockHash]] =
    (for {
      block <- OptionT(blockDag.lookup(blockHash))
      creatorJustificationHash <- OptionT.fromOption(
                                   block.justifications
                                     .find(_.validator == block.sender)
                                     .map(_.latestBlockHash)
                                 )
      creatorJustification <- OptionT(blockDag.lookup(creatorJustificationHash))
      creatorJustificationAsList = if (goalFunc(creatorJustification.blockHash)) {
        List.empty[BlockHash]
      } else {
        List(creatorJustification.blockHash)
      }
    } yield creatorJustificationAsList).fold(List.empty[BlockHash])(identity)

  def weightMap(blockMessage: BlockMessage): Map[ByteString, Long] =
    weightMap(blockMessage.bonds)

  private def weightMap(bonds: Seq[Bond]): Map[ByteString, Long] =
    bonds.map {
      case Bond(validator, stake) => validator -> stake
    }.toMap

  def weightMapTotal(weights: Map[ByteString, Long]): Long =
    weights.values.sum

  def minTotalValidatorWeight[F[_]: Monad](
      blockDag: BlockDagRepresentation[F],
      blockHash: BlockHash,
      maxCliqueMinSize: Int
  ): F[Long] =
    blockDag.lookup(blockHash).map { blockMetadataOpt =>
      val sortedWeights = blockMetadataOpt.get.weightMap.values.toVector.sorted
      sortedWeights.take(maxCliqueMinSize).sum
    }

  def mainParent[F[_]: Monad: BlockStore](blockMessage: BlockMessage): F[Option[BlockMessage]] = {
    import cats.instances.option._
    blockMessage.header.parentsHashList.headOption.flatTraverse(BlockStore[F].get1)
  }

  def weightFromValidatorByDag[F[_]: Monad](
      dag: BlockDagRepresentation[F],
      blockHash: BlockHash,
      validator: Validator
  ): F[Long] = {
    import cats.instances.option._

    for {
      blockMetadata  <- dag.lookup(blockHash)
      blockParentOpt = blockMetadata.get.parents.headOption
      resultOpt <- blockParentOpt.traverse { bh =>
                    dag.lookup(bh).map(_.get.weightMap.getOrElse(validator, 0L))
                  }
      result <- resultOpt match {
                 case Some(result) => result.pure[F]
                 case None         => dag.lookup(blockHash).map(_.get.weightMap.getOrElse(validator, 0L))
               }
    } yield result
  }

  def weightFromValidator[F[_]: Monad: BlockStore](
      b: BlockMessage,
      validator: ByteString
  ): F[Long] =
    for {
      maybeMainParent <- mainParent(b)
      weightFromValidator = maybeMainParent
        .map(weightMap(_).getOrElse(validator, 0L))
        .getOrElse(weightMap(b).getOrElse(validator, 0L)) //no parents means genesis -- use itself
    } yield weightFromValidator

  def weightFromSender[F[_]: Monad: BlockStore](b: BlockMessage): F[Long] =
    weightFromValidator(b, b.sender)

  def parentHashes(b: BlockMessage): List[ByteString] =
    b.header.parentsHashList

  def getParents[F[_]: Sync: BlockStore](b: BlockMessage): F[List[BlockMessage]] = {
    import cats.instances.list._
    parentHashes(b).traverse(BlockStore[F].getUnsafe)
  }

  def getParentsMetadata[F[_]: Sync](
      b: BlockMetadata,
      dag: BlockDagRepresentation[F]
  ): F[List[BlockMetadata]] = {
    import cats.instances.list._
    b.parents.traverse(dag.lookupUnsafe)
  }

  def getParentMetadatasAboveBlockNumber[F[_]: Sync](
      b: BlockMetadata,
      blockNumber: Long,
      dag: BlockDagRepresentation[F]
  ): F[List[BlockMetadata]] =
    getParentsMetadata(b, dag)
      .map(parents => parents.filter(p => p.blockNum >= blockNumber))

  def deploys(b: BlockMessage): Seq[ProcessedDeploy] =
    b.state.deploys

  def systemDeploys(b: BlockMessage): Seq[ProcessedSystemDeploy] =
    b.state.systemDeploys

  def postStateHash(b: BlockMessage): ByteString =
    b.postStateHash

  def preStateHash(b: BlockMessage): ByteString =
    b.preStateHash

  def bonds(b: BlockMessage): Seq[Bond] =
    b.bonds

  def blockNumber(b: BlockMessage): Long =
    b.blockNumber

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

  def toLatestMessage[F[_]: Sync: BlockStore](
      justifications: Seq[Justification],
      dag: BlockDagRepresentation[F]
  ): F[immutable.Map[Validator, BlockMetadata]] = {

    import cats.instances.list._
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
  }

  def protoSeqHash[A <: { def toByteArray: Array[Byte] }](protoSeq: Seq[A]): ByteString =
    hashByteArrays(protoSeq.map(_.toByteArray): _*)

  def hashByteArrays(items: Array[Byte]*): ByteString =
    ByteString.copyFrom(Blake2b256.hash(Array.concat(items: _*)))

  // TODO inline this
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
      state: RholangTrace,
      preStateHash: ByteString,
      postStateHash: ByteString,
      bonds: List[Bond],
      header: Header,
      justifications: Seq[Justification],
      shardId: String,
      blockNumber: Long,
      seqNum: Int = 0
  ): BlockMessage = {
    // TODO FIX-ME fields that can be empty SHOULD be optional
    val block = BlockMessage(
      blockHash = ByteString.EMPTY,
      blockNumber,
      bonds,
      header,
      preStateHash,
      postStateHash,
      state,
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
      blockMessage.state.toProto.toByteArray,
      blockMessage.sender.toByteArray,
      StringValue.of(blockMessage.sigAlgorithm).toByteArray,
      Int32Value.of(blockMessage.seqNum).toByteArray,
      StringValue.of(blockMessage.shardId).toByteArray,
      blockMessage.extraBytes.toByteArray
    )
  def hashString(b: BlockMessage): String = Base16.encode(b.blockHash.toByteArray)

  def computeCodeHash(dd: DeployData): Par = {
    val bytes             = dd.term.getBytes(StandardCharsets.UTF_8)
    val hash: Array[Byte] = Blake2b256.hash(bytes)
    Par(exprs = Seq(Expr(Expr.ExprInstance.GByteArray(ByteString.copyFrom(hash)))))
  }

  def getRholangDeployParams(dd: Signed[DeployData]): DeployParameters = {
    val userId: Par = Par(
      exprs = Seq(Expr(Expr.ExprInstance.GByteArray(ByteString.copyFrom(dd.pk.bytes))))
    )
    DeployParameters(userId)
  }

  def dependenciesHashesOf(b: BlockMessage): List[BlockHash] = {
    val missingParents = parentHashes(b).toSet
    val missingJustifications = b.justifications
      .map(_.latestBlockHash)
      .toSet
    (missingParents union missingJustifications).toList
  }

  // Return hashes of all blocks that are yet to be seen by the passed in block
  def unseenBlockHashes[F[_]: Sync: BlockStore](
      dag: BlockDagRepresentation[F],
      block: BlockMessage
  ): F[Set[BlockHash]] = {
    import cats.instances.stream._

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
  }

  private def getCreatorBlocksBetween[F[_]: Sync](
      dag: BlockDagRepresentation[F],
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

  private def getCreatorJustificationUnlessGoal[F[_]: Sync](
      dag: BlockDagRepresentation[F],
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
