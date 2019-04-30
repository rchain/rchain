package coop.rchain.casper.util

import java.nio.charset.StandardCharsets

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.{ByteString, Int32Value, StringValue}
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockStore}
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.{DeployData, _}
import coop.rchain.casper.util.implicits._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.DeployParameters

import scala.collection.immutable

object ProtoUtil {

  /*
   * c is in the blockchain of b iff c == b or c is in the blockchain of the main parent of b
   */
  // TODO: Move into BlockDAG and remove corresponding param once that is moved over from simulator
  def isInMainChain[F[_]: Monad](
      dag: BlockDagRepresentation[F],
      candidateBlockHash: BlockHash,
      targetBlockHash: BlockHash
  ): F[Boolean] =
    if (candidateBlockHash == targetBlockHash) {
      true.pure[F]
    } else {
      for {
        targetBlockOpt <- dag.lookup(targetBlockHash)
        result <- targetBlockOpt match {
                   case Some(targetBlockMeta) =>
                     targetBlockMeta.parents.headOption match {
                       case Some(mainParentHash) =>
                         isInMainChain(dag, candidateBlockHash, mainParentHash)
                       case None => false.pure[F]
                     }
                   case None => false.pure[F]
                 }
      } yield result
    }

  def getMainChainUntilDepth[F[_]: Monad: BlockStore](
      estimate: BlockMessage,
      acc: IndexedSeq[BlockMessage],
      depth: Int
  ): F[IndexedSeq[BlockMessage]] = {
    val parentsHashes       = ProtoUtil.parentHashes(estimate)
    val maybeMainParentHash = parentsHashes.headOption
    for {
      mainChain <- maybeMainParentHash match {
                    case Some(mainParentHash) =>
                      for {
                        updatedEstimate <- unsafeGetBlock[F](mainParentHash)
                        depthDelta      = blockNumber(updatedEstimate) - blockNumber(estimate)
                        newDepth        = depth + depthDelta.toInt
                        mainChain <- if (newDepth <= 0) {
                                      (acc :+ estimate).pure[F]
                                    } else {
                                      getMainChainUntilDepth[F](
                                        updatedEstimate,
                                        acc :+ estimate,
                                        newDepth
                                      )
                                    }
                      } yield mainChain
                    case None => (acc :+ estimate).pure[F]
                  }
    } yield mainChain
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw")) // TODO remove throw
  def unsafeGetBlock[F[_]: Monad: BlockStore](hash: BlockHash): F[BlockMessage] =
    for {
      maybeBlock <- BlockStore[F].get(hash)
      block = maybeBlock match {
        case Some(b) => b
        case None =>
          throw new Exception(s"BlockStore is missing hash ${PrettyPrinter.buildString(hash)}")
      }
    } yield block

  def creatorJustification(block: BlockMessage): Option[Justification] =
    block.justifications
      .find {
        case Justification(validator: Validator, _) =>
          validator == block.sender
      }

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
      creatorJustificationHash <- OptionT.fromOption[F](
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
    blockMessage.body match {
      case Some(block) =>
        block.state match {
          case Some(state) => weightMap(state)
          case None        => Map.empty[ByteString, Long]
        }
      case None => Map.empty[ByteString, Long]
    }

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
    val maybeParentHash = for {
      hdr        <- blockMessage.header
      parentHash <- hdr.parentsHashList.headOption
    } yield parentHash
    maybeParentHash match {
      case Some(parentHash) => BlockStore[F].get(parentHash)
      case None             => none[BlockMessage].pure[F]
    }
  }

  def weightFromValidatorByDag[F[_]: Monad](
      dag: BlockDagRepresentation[F],
      blockHash: BlockHash,
      validator: Validator
  ): F[Long] =
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

  def weightFromValidator[F[_]: Monad: BlockStore](
      b: BlockMessage,
      validator: ByteString
  ): F[Long] =
    for {
      maybeMainParent <- mainParent[F](b)
      weightFromValidator = maybeMainParent
        .map(weightMap(_).getOrElse(validator, 0L))
        .getOrElse(weightMap(b).getOrElse(validator, 0L)) //no parents means genesis -- use itself
    } yield weightFromValidator

  def weightFromSender[F[_]: Monad: BlockStore](b: BlockMessage): F[Long] =
    weightFromValidator[F](b, b.sender)

  def parentHashes(b: BlockMessage): Seq[ByteString] =
    b.header.fold(Seq.empty[ByteString])(_.parentsHashList)

  def unsafeGetParents[F[_]: Monad: BlockStore](b: BlockMessage): F[List[BlockMessage]] =
    ProtoUtil.parentHashes(b).toList.traverse { parentHash =>
      ProtoUtil.unsafeGetBlock[F](parentHash)
    }

  def unsafeGetParentsAboveBlockNumber[F[_]: Monad: BlockStore](
      b: BlockMessage,
      blockNumber: Long
  ): F[List[BlockMessage]] =
    ProtoUtil
      .unsafeGetParents[F](b)
      .map(parents => parents.filter(p => ProtoUtil.blockNumber(p) >= blockNumber))

  def containsDeploy(b: BlockMessage, user: ByteString, timestamp: Long): Boolean =
    deploys(b).toStream
      .flatMap(getDeployData)
      .exists(deployData => deployData.deployer == user && deployData.timestamp == timestamp)

  private def getDeployData(d: ProcessedDeploy): Option[DeployData] = d.deploy

  def deploys(b: BlockMessage): Seq[ProcessedDeploy] =
    b.body.fold(Seq.empty[ProcessedDeploy])(_.deploys)

  def tuplespace(b: BlockMessage): Option[ByteString] =
    for {
      bd <- b.body
      ps <- bd.state
    } yield ps.postStateHash

  // TODO: Reconcile with def tuplespace above
  def postStateHash(b: BlockMessage): ByteString =
    b.getBody.getState.postStateHash

  def preStateHash(b: BlockMessage): ByteString =
    b.getBody.getState.preStateHash

  def bonds(b: BlockMessage): Seq[Bond] =
    (for {
      bd <- b.body
      ps <- bd.state
    } yield ps.bonds).getOrElse(List.empty[Bond])

  def blockNumber(b: BlockMessage): Long =
    (for {
      bd <- b.body
      ps <- bd.state
    } yield ps.blockNumber).getOrElse(0L)

  def maxBlockNumber(blocks: Seq[BlockMessage]): Long = blocks.foldLeft(-1L) {
    case (acc, b) => math.max(acc, ProtoUtil.blockNumber(b))
  }

  def toJustification(
      latestMessages: collection.Map[Validator, BlockMetadata]
  ): Seq[Justification] =
    latestMessages.toSeq.map {
      case (validator, blockMetadata) =>
        Justification()
          .withValidator(validator)
          .withLatestBlockHash(blockMetadata.blockHash)
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
                                  s"Could not find a block for $hash in the DAG storage"
                                )
                              )
                            )
        } yield acc.updated(validator, blockMetadata)
    }

  def protoHash[A <: { def toByteArray: Array[Byte] }](protoSeq: A*): ByteString =
    protoSeqHash(protoSeq)

  def protoSeqHash[A <: { def toByteArray: Array[Byte] }](protoSeq: Seq[A]): ByteString =
    hashByteArrays(protoSeq.map(_.toByteArray): _*)

  def hashByteArrays(items: Array[Byte]*): ByteString =
    ByteString.copyFrom(Blake2b256.hash(Array.concat(items: _*)))

  def blockHeader(
      body: Body,
      parentHashes: Seq[ByteString],
      version: Long,
      timestamp: Long
  ): Header =
    Header()
      .withParentsHashList(parentHashes)
      .withPostStateHash(protoHash(body.state.get))
      .withDeploysHash(protoSeqHash(body.deploys))
      .withDeployCount(body.deploys.size)
      .withVersion(version)
      .withTimestamp(timestamp)

  def unsignedBlockProto(
      body: Body,
      header: Header,
      justifications: Seq[Justification],
      shardId: String
  ): BlockMessage = {
    val hash = hashUnsignedBlock(header, justifications)

    BlockMessage()
      .withBlockHash(hash)
      .withHeader(header)
      .withBody(body)
      .withJustifications(justifications)
      .withShardId(shardId)
  }

  def hashUnsignedBlock(header: Header, justifications: Seq[Justification]): BlockHash = {
    val items = header.toByteArray +: justifications.map(_.toByteArray)
    hashByteArrays(items: _*)
  }

  def hashSignedBlock(
      header: Header,
      sender: ByteString,
      sigAlgorithm: String,
      seqNum: Int,
      shardId: String,
      extraBytes: ByteString
  ): BlockHash =
    hashByteArrays(
      header.toByteArray,
      sender.toByteArray,
      StringValue.of(sigAlgorithm).toByteArray,
      Int32Value.of(seqNum).toByteArray,
      StringValue.of(shardId).toByteArray,
      extraBytes.toByteArray
    )

  def signBlock[F[_]: Applicative](
      block: BlockMessage,
      dag: BlockDagRepresentation[F],
      pk: PublicKey,
      sk: PrivateKey,
      sigAlgorithm: String,
      shardId: String
  ): F[BlockMessage] = {

    val header = {
      //TODO refactor casper code to avoid the usage of Option fields in the block data structures
      // https://rchain.atlassian.net/browse/RHOL-572
      assert(block.header.isDefined, "A block without a header doesn't make sense")
      block.header.get
    }

    val sender = ByteString.copyFrom(pk.bytes)
    for {
      latestMessageOpt  <- dag.latestMessage(sender)
      seqNum            = latestMessageOpt.fold(0)(_.seqNum) + 1
      blockHash         = hashSignedBlock(header, sender, sigAlgorithm, seqNum, shardId, block.extraBytes)
      sigAlgorithmBlock = block.withSigAlgorithm(sigAlgorithm)
      sig               = ByteString.copyFrom(sigAlgorithmBlock.signFunction(blockHash.toByteArray, sk))
      signedBlock = sigAlgorithmBlock
        .withSender(sender)
        .withSig(sig)
        .withSeqNum(seqNum)
        .withBlockHash(blockHash)
        .withShardId(shardId)
    } yield signedBlock
  }

  def hashString(b: BlockMessage): String = Base16.encode(b.blockHash.toByteArray)

  def stringToByteString(string: String): ByteString =
    ByteString.copyFrom(Base16.unsafeDecode(string))

  /**
    * Strip a deploy down to the fields we are using to seed the Deterministic name generator.
    * Because we enforce that a deployment must be unique on the user, timestamp pair, we leave
    * only those fields. This allows users to more readily pre-generate names for signing.
    */
  def stripDeployData(d: DeployData): DeployData =
    DeployData().withDeployer(d.deployer).withTimestamp(d.timestamp)

  def computeCodeHash(dd: DeployData): Par = {
    val bytes             = dd.term.getBytes(StandardCharsets.UTF_8)
    val hash: Array[Byte] = Blake2b256.hash(bytes)
    Par(exprs = Seq(Expr(Expr.ExprInstance.GByteArray(ByteString.copyFrom(hash)))))
  }

  def getRholangDeployParams(dd: DeployData): DeployParameters = {
    val phloPrice: Par = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(dd.phloPrice))))
    val userId: Par    = Par(exprs = Seq(Expr(Expr.ExprInstance.GByteArray(dd.deployer))))
    val timestamp: Par = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(dd.timestamp))))
    DeployParameters(computeCodeHash(dd), phloPrice, userId, timestamp)
  }

  def dependenciesHashesOf(b: BlockMessage): List[BlockHash] = {
    val missingParents = parentHashes(b).toSet
    val missingJustifications = b.justifications
      .map(_.latestBlockHash)
      .toSet
    (missingParents union missingJustifications).toList
  }

}
