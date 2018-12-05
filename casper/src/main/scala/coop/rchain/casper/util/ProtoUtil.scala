package coop.rchain.casper.util

import cats.{Applicative, Monad}
import cats.implicits._
import com.google.protobuf.{ByteString, Int32Value, StringValue}
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockMetadata, BlockStore}
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.EquivocationRecord.SequenceNumber
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.{DeployData, _}
import coop.rchain.casper.util.ProtoUtil.basicDeployData
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.casper.util.implicits._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.models._
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.shared.Time
import java.nio.charset.StandardCharsets

import coop.rchain.casper.protocol.Event.EventInstance.{Comm, Consume, Produce}

import scala.collection.{immutable, BitSet}

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

  def findCreatorJustificationAncestorWithSeqNum[F[_]: Monad: BlockStore](
      b: BlockMessage,
      seqNum: SequenceNumber
  ): F[Option[BlockMessage]] =
    if (b.seqNum == seqNum) {
      Option[BlockMessage](b).pure[F]
    } else {
      DagOperations
        .bfTraverseF(List(b)) { block =>
          getCreatorJustificationAsList[F](block, block.sender)
        }
        .find(_.seqNum == seqNum)
    }

  def getCreatorJustificationAsList[F[_]: Monad: BlockStore](
      block: BlockMessage,
      validator: Validator,
      goalFunc: BlockMessage => Boolean = _ => false
  ): F[List[BlockMessage]] = {
    val maybeCreatorJustificationHash =
      block.justifications.find(_.validator == validator)
    maybeCreatorJustificationHash match {
      case Some(creatorJustificationHash) =>
        for {
          maybeCreatorJustification <- BlockStore[F].get(creatorJustificationHash.latestBlockHash)
          maybeCreatorJustificationAsList = maybeCreatorJustification match {
            case Some(creatorJustification) =>
              if (goalFunc(creatorJustification)) {
                List.empty[BlockMessage]
              } else {
                List(creatorJustification)
              }
            case None =>
              List.empty[BlockMessage]
          }
        } yield maybeCreatorJustificationAsList
      case None => List.empty[BlockMessage].pure[F]
    }
  }

  def getCreatorJustificationAsListByInMemory[F[_]: Monad](
      blockDag: BlockDagRepresentation[F],
      blockHash: BlockHash,
      validator: Validator,
      goalFunc: BlockHash => Boolean = _ => false
  ): F[List[BlockHash]] =
    for {
      maybeCreatorBlock <- blockDag.lookup(blockHash)
      maybeCreatorJustificationHash = maybeCreatorBlock.flatMap(
        _.justifications.find(_.validator == validator)
      )
      result <- maybeCreatorJustificationHash match {
                 case Some(creatorJustificationHash) =>
                   for {
                     maybeCreatorJustification <- blockDag.lookup(
                                                   creatorJustificationHash.latestBlockHash
                                                 )
                     result = maybeCreatorJustification match {
                       case Some(creatorJustification) =>
                         if (goalFunc(creatorJustification.blockHash)) {
                           List.empty[BlockHash]
                         } else {
                           List(creatorJustification.blockHash)
                         }
                       case None => List.empty[BlockHash]
                     }
                   } yield result
                 case None => List.empty[BlockHash].pure[F]
               }
    } yield result

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

  def containsDeploy(b: BlockMessage, user: ByteString, timestamp: Long): Boolean =
    deploys(b).toStream
      .flatMap(getDeployData)
      .exists(deployData => deployData.user == user && deployData.timestamp == timestamp)

  private def getDeployData(d: ProcessedDeploy): Option[DeployData] =
    for {
      deploy     <- d.deploy
      deployData <- deploy.raw
    } yield deployData

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

  /*
   * Block b1 conflicts with b2 if any of b1's ancestors contains a replay log entry that
   * touches a channel that any of b2's ancestors' (that are not common with b1's ancestors)
   * replay log entries touch.
   */
  def conflicts[F[_]: Monad: BlockStore](
      b1: BlockMessage,
      b2: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[Boolean] =
    dag.deriveOrdering(0L).flatMap { // TODO: Replace with something meaningful
      implicit ordering =>
        for {
          b1MetaDataOpt        <- dag.lookup(b1.blockHash)
          b2MetaDataOpt        <- dag.lookup(b2.blockHash)
          blockMetaDataSeq     = Vector(b1MetaDataOpt.get, b2MetaDataOpt.get)
          uncommonAncestorsMap <- DagOperations.uncommonAncestors(blockMetaDataSeq, dag)
          (b1AncestorsMap, b2AncestorsMap) = uncommonAncestorsMap.partition {
            case (_, bitSet) => bitSet == BitSet(0)
          }
          b1AncestorsMeta    = b1AncestorsMap.keys
          b2AncestorsMeta    = b2AncestorsMap.keys
          b1AncestorChannels <- buildBlockAncestorChannels[F](b1AncestorsMeta.toList)
          b2AncestorChannels <- buildBlockAncestorChannels[F](b2AncestorsMeta.toList)
        } yield b1AncestorChannels.intersect(b2AncestorChannels).nonEmpty
    }

  private def buildBlockAncestorChannels[F[_]: Monad: BlockStore](
      blockAncestorsMeta: List[BlockMetadata]
  ): F[Set[BlockHash]] =
    for {
      maybeAncestors <- blockAncestorsMeta.traverse(
                         blockAncestorMeta => BlockStore[F].get(blockAncestorMeta.blockHash)
                       )
      ancestors      = maybeAncestors.flatten
      ancestorEvents = ancestors.flatMap(_.getBody.deploys.flatMap(_.log))
      ancestorChannels = ancestorEvents.flatMap {
        case Event(Produce(produce: ProduceEvent)) =>
          Set(produce.channelsHash)
        case Event(Consume(consume: ConsumeEvent)) =>
          consume.channelsHashes.toSet
        case Event(Comm(CommEvent(Some(consume: ConsumeEvent), produces))) =>
          consume.channelsHashes.toSet ++ produces.map(_.channelsHash).toSet
      }.toSet
    } yield ancestorChannels

  def chooseNonConflicting[F[_]: Monad: BlockStore](
      blocks: Seq[BlockMessage],
      genesis: BlockMessage,
      dag: BlockDagRepresentation[F]
  ): F[Seq[BlockMessage]] = {
    def nonConflicting(b: BlockMessage): BlockMessage => F[Boolean] =
      conflicts[F](_, b, dag).map(b => !b)

    blocks.toList
      .foldM(List.empty[BlockMessage]) {
        case (acc, b) =>
          Monad[F].ifM(acc.forallM(nonConflicting(b)))(
            (b :: acc).pure[F],
            acc.pure[F]
          )
      }
      .map(_.reverse)
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

  def toLatestMessage[F[_]: Monad: BlockStore](
      justifications: Seq[Justification],
      dag: BlockDagRepresentation[F]
  ): F[immutable.Map[Validator, BlockMetadata]] =
    justifications.toList.foldM(Map.empty[Validator, BlockMetadata]) {
      case (acc, Justification(validator, hash)) =>
        for {
          block <- ProtoUtil.unsafeGetBlock[F](hash)
        } yield acc.updated(validator, BlockMetadata.fromBlock(block))
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
      pk: Array[Byte],
      sk: Array[Byte],
      sigAlgorithm: String,
      shardId: String
  ): F[BlockMessage] = {

    val header = {
      //TODO refactor casper code to avoid the usage of Option fields in the block data structures
      // https://rchain.atlassian.net/browse/RHOL-572
      assert(block.header.isDefined, "A block without a header doesn't make sense")
      block.header.get
    }

    val sender = ByteString.copyFrom(pk)
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
    ByteString.copyFrom(Base16.decode(string))

  def basicDeployData[F[_]: Monad: Time](id: Int): F[DeployData] =
    Time[F].currentMillis.map(
      now =>
        DeployData()
          .withUser(ByteString.EMPTY)
          .withTimestamp(now)
          .withTerm(s"@${id}!($id)")
          .withPhloLimit(accounting.MAX_VALUE)
    )

  def basicDeploy[F[_]: Monad: Time](id: Int): F[Deploy] =
    for {
      d    <- basicDeployData[F](id)
      term = InterpreterUtil.mkTerm(d.term).right.get
    } yield Deploy(term = Some(term), raw = Some(d))

  def basicProcessedDeploy[F[_]: Monad: Time](id: Int): F[ProcessedDeploy] =
    basicDeploy[F](id).map(deploy => ProcessedDeploy(deploy = Some(deploy)))

  def sourceDeploy(source: String, timestamp: Long, phlos: Long): DeployData =
    DeployData(
      user = ByteString.EMPTY,
      timestamp = timestamp,
      term = source,
      phloLimit = phlos
    )

  def compiledSourceDeploy(
      source: CompiledRholangSource,
      timestamp: Long,
      phloLimit: Long
  ): Deploy =
    Deploy(
      term = Some(source.term),
      raw = Some(sourceDeploy(source.code, timestamp, phloLimit))
    )

  def termDeploy(term: Par, timestamp: Long, phloLimit: Long): Deploy =
    Deploy(
      term = Some(term),
      raw = Some(
        DeployData(
          user = ByteString.EMPTY,
          timestamp = timestamp,
          term = term.toProtoString,
          phloLimit = phloLimit
        )
      )
    )

  def termDeployNow(term: Par): Deploy =
    termDeploy(term, System.currentTimeMillis(), accounting.MAX_VALUE)

  def deployDataToDeploy(dd: DeployData): Deploy = Deploy(
    term = InterpreterUtil.mkTerm(dd.term).toOption,
    raw = Some(dd)
  )

  /**
    * Strip a deploy down to the fields we are using to seed the Deterministic name generator.
    * Because we enforce that a deployment must be unique on the user, timestamp pair, we leave
    * only those fields. This allows users to more readily pre-generate names for signing.
    */
  def stripDeployData(d: DeployData): DeployData =
    DeployData().withUser(d.user).withTimestamp(d.timestamp)

  def computeCodeHash(dd: DeployData): Par = {
    val bytes             = dd.term.getBytes(StandardCharsets.UTF_8)
    val hash: Array[Byte] = Blake2b256.hash(bytes)
    Par(exprs = Seq(Expr(Expr.ExprInstance.GByteArray(ByteString.copyFrom(hash)))))
  }

  def getRholangDeployParams(dd: DeployData): (Par, Par, Par, Par) = {
    val phloPrice: Par = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(dd.phloPrice))))
    val userId: Par    = Par(exprs = Seq(Expr(Expr.ExprInstance.GByteArray(dd.user))))
    val timestamp: Par = Par(exprs = Seq(Expr(Expr.ExprInstance.GInt(dd.timestamp))))
    (computeCodeHash(dd), phloPrice, userId, timestamp)
  }
}
