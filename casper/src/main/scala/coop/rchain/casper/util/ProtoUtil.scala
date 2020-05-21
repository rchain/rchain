package coop.rchain.casper.util

import java.nio.charset.StandardCharsets

import cats.data.OptionT
import cats.effect.Sync
import cats.syntax.all._
import cats.{Applicative, Monad}
import com.google.protobuf.{ByteString, Int32Value, StringValue}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.{DeployData, _}
import coop.rchain.casper.util.implicits._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Signed
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models._
import coop.rchain.rholang.interpreter.DeployParameters

import scala.collection.immutable
import scala.collection.immutable.Map

object ProtoUtil {

  /*
   * c is in the blockchain of b iff c == b or c is in the blockchain of the main parent of b
   */
  def isInMainChain[F[_]: Monad](
      dag: BlockDagRepresentation[F],
      candidateMetadata: BlockMetadata,
      targetBlockHash: BlockHash
  ): F[Boolean] = {
    import coop.rchain.catscontrib.Catscontrib.ToBooleanF
    import cats.instances.option._

    (candidateMetadata.blockHash == targetBlockHash).pure[F] ||^
      dag.lookup(targetBlockHash).flatMap {
        case Some(targetBlock) =>
          if (targetBlock.blockNum <= candidateMetadata.blockNum) {
            false.pure[F]
          } else {
            val mainParentOpt = targetBlock.parents.headOption
            mainParentOpt.traverse(isInMainChain(dag, candidateMetadata, _)).map(_.getOrElse(false))
          }
        case None =>
          false.pure[F]
      }
  }

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
                        updatedEstimate <- getBlock(mainParentHash)
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

  def getBlock[F[_]: Sync: BlockStore](hash: BlockHash): F[BlockMessage] =
    BlockStore[F].get(hash) >>= (Sync[F].fromOption(
      _,
      new Exception(s"BlockStore is missing hash ${PrettyPrinter.buildString(hash)}")
    ))

  def getBlockMetadata[F[_]: Sync](
      hash: BlockHash,
      dag: BlockDagRepresentation[F]
  ): F[BlockMetadata] =
    dag.lookup(hash) >>= (
      Sync[F].fromOption(
        _,
        new Exception(s"DAG storage is missing hash ${PrettyPrinter.buildString(hash)}")
      )
    )

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
    weightMap(blockMessage.body.state)

  private def weightMap(state: RChainState): Map[ByteString, Long] =
    state.bonds.map {
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
    blockMessage.header.parentsHashList.headOption.flatTraverse(BlockStore[F].get)
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
    parentHashes(b).traverse(getBlock[F])
  }

  def getParentsMetadata[F[_]: Sync](
      b: BlockMetadata,
      dag: BlockDagRepresentation[F]
  ): F[List[BlockMetadata]] = {
    import cats.instances.list._
    b.parents.traverse(getBlockMetadata(_, dag))
  }

  def getParentMetadatasAboveBlockNumber[F[_]: Sync](
      b: BlockMetadata,
      blockNumber: Long,
      dag: BlockDagRepresentation[F]
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
                                  s"Could not find a block for $hash in the DAG storage"
                                )
                              )
                            )
        } yield acc.updated(validator, blockMetadata)
    }
  }

  def protoSeqHash[A <: { def toByteArray: Array[Byte] }](protoSeq: Seq[A]): ByteString =
    hashByteArrays(protoSeq.map(_.toByteArray): _*)

  private def hashByteArrays(items: Array[Byte]*): ByteString =
    ByteString.copyFrom(Blake2b256.hash(Array.concat(items: _*)))

  // TODO inline this
  def blockHeader(
      body: Body,
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
      shardId: String
  ): BlockMessage = {
    val hash = hashUnsignedBlock(header, justifications)

    // TODO FIX-ME fields that can be empty SHOULD be optional
    BlockMessage(
      hash,
      header,
      body,
      justifications.toList,
      sender = ByteString.EMPTY,
      seqNum = 0,
      sig = ByteString.EMPTY,
      sigAlgorithm = "",
      shardId,
      extraBytes = ByteString.EMPTY
    )
  }

  def hashUnsignedBlock(header: Header, justifications: Seq[Justification]): BlockHash = {
    val items = header.toProto.toByteArray +: justifications.map(_.toProto.toByteArray)
    hashByteArrays(items: _*)
  }

  def hashSignedBlock(
      header: Header,
      body: Body,
      sender: ByteString,
      sigAlgorithm: String,
      seqNum: Int,
      shardId: String,
      extraBytes: ByteString
  ): BlockHash =
    hashByteArrays(
      header.toProto.toByteArray,
      body.toProto.toByteArray,
      sender.toByteArray,
      StringValue.of(sigAlgorithm).toByteArray,
      Int32Value.of(seqNum).toByteArray,
      StringValue.of(shardId).toByteArray,
      extraBytes.toByteArray
    )

  def signBlock(
      block: BlockMessage,
      sk: PrivateKey,
      sigAlgorithm: String,
      shardId: String,
      seqNum: Int,
      sender: Validator
  ): BlockMessage = {

    val header = block.header
    val blockHash = hashSignedBlock(
      header,
      block.body,
      sender,
      sigAlgorithm,
      seqNum,
      shardId,
      block.extraBytes
    )
    val sigAlgorithmBlock = block.copy(sigAlgorithm = sigAlgorithm)
    val sig               = ByteString.copyFrom(sigAlgorithmBlock.signFunction(blockHash.toByteArray, sk))
    sigAlgorithmBlock.copy(
      sender = sender,
      sig = sig,
      seqNum = seqNum,
      blockHash = blockHash,
      shardId = shardId
    )
  }

  def hashString(b: BlockMessage): String = Base16.encode(b.blockHash.toByteArray)

  def stringToByteString(string: String): ByteString =
    ByteString.copyFrom(Base16.unsafeDecode(string))

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
      latestMessages        <- dag.latestMessages
      latestMessagesOfBlock <- toLatestMessage(block.justifications, dag)
      unseenBlockHashesAndLatestMessages <- latestMessages.toStream
                                             .traverse {
                                               case (validator, latestMessage) =>
                                                 getJustificationChainFromLatestMessageToBlock(
                                                   dag,
                                                   latestMessagesOfBlock,
                                                   validator,
                                                   latestMessage
                                                 )
                                             }
                                             .map(_.flatten.toSet)
    } yield unseenBlockHashesAndLatestMessages -- latestMessagesOfBlock.values.map(_.blockHash) - block.blockHash
  }

  private def getJustificationChainFromLatestMessageToBlock[F[_]: Sync: BlockStore](
      dag: BlockDagRepresentation[F],
      latestMessagesOfBlock: Map[Validator, BlockMetadata],
      validator: Validator,
      latestMessage: BlockMetadata
  ): F[Set[BlockHash]] =
    latestMessagesOfBlock.get(validator) match {
      case Some(latestMessageOfBlockByValidator) =>
        DagOperations
          .bfTraverseF(List(latestMessage))(
            block =>
              getCreatorJustificationUnlessGoal(
                dag,
                block,
                latestMessageOfBlockByValidator
              )
          )
          .map(_.blockHash)
          .toSet
      case None =>
        Set.empty[BlockHash].pure
    }

  private def getCreatorJustificationUnlessGoal[F[_]: Sync: BlockStore](
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
              new RuntimeException(s"Missing block hash $hash in block dag.")
            )
        }
      case None =>
        List.empty[BlockMetadata].pure
    }

  def invalidLatestMessages[F[_]: Monad](
      dag: BlockDagRepresentation[F]
  ): F[Map[Validator, BlockHash]] =
    dag.latestMessages.flatMap(
      latestMessages =>
        invalidLatestMessages(dag, latestMessages.map {
          case (validator, block) => (validator, block.blockHash)
        })
    )

  def invalidLatestMessages[F[_]: Monad](
      dag: BlockDagRepresentation[F],
      latestMessagesHashes: Map[Validator, BlockHash]
  ): F[Map[Validator, BlockHash]] =
    dag.invalidBlocks.map { invalidBlocks =>
      latestMessagesHashes.filter {
        case (_, blockHash) => invalidBlocks.map(_.blockHash).contains(blockHash)
      }
    }

  def slashedInvalidValidators[F[_]: Monad](
      dag: BlockDagRepresentation[F]
  ): F[Set[Validator]] = dag.slashedInvalidValidators
}
