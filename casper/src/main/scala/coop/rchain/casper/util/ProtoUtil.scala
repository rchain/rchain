package coop.rchain.casper.util

import cats.Monad
import cats.implicits._
import com.google.protobuf.{ByteString, Int32Value, StringValue}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.{BlockDag, PrettyPrinter}
import coop.rchain.casper.EquivocationRecord.SequenceNumber
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.casper.util.implicits._
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.models.{PCost, Par}
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.accounting

import scala.collection.immutable

object ProtoUtil {
  /*
   * c is in the blockchain of b iff c == b or c is in the blockchain of the main parent of b
   */
  // TODO: Move into BlockDAG and remove corresponding param once that is moved over from simulator
  def isInMainChain(
      dag: BlockDag,
      candidateBlockHash: BlockHash,
      targetBlockHash: BlockHash
  ): Boolean =
    if (candidateBlockHash == targetBlockHash) {
      true
    } else {
      dag.dataLookup.get(targetBlockHash) match {
        case Some(targetBlockMeta) =>
          targetBlockMeta.parents.headOption match {
            case Some(mainParentHash) => isInMainChain(dag, candidateBlockHash, mainParentHash)
            case None                 => false
          }
        case None => false
      }
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

  def getCreatorJustificationAsListByInMemory(
      blockDag: BlockDag,
      blockHash: BlockHash,
      validator: Validator,
      goalFunc: BlockHash => Boolean = _ => false
  ): List[BlockHash] = {
    val maybeCreatorJustificationHash =
      blockDag.dataLookup(blockHash).justifications.find(_.validator == validator)
    maybeCreatorJustificationHash match {
      case Some(creatorJustificationHash) =>
        blockDag.dataLookup.get(creatorJustificationHash.latestBlockHash) match {
          case Some(creatorJustification) =>
            if (goalFunc(creatorJustification.blockHash)) {
              List.empty[BlockHash]
            } else {
              List(creatorJustification.blockHash)
            }
          case None =>
            List.empty[BlockHash]
        }
      case None => List.empty[BlockHash]
    }
  }

  def weightMap(blockMessage: BlockMessage): Map[ByteString, Long] =
    blockMessage.body match {
      case Some(block) =>
        block.postState match {
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

  def minTotalValidatorWeight(dag: BlockDag, blockHash: BlockHash, maxCliqueMinSize: Int): Long = {
    val sortedWeights = dag.dataLookup(blockHash).weightMap.values.toVector.sorted
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

  def weightFromValidatorByDag(dag: BlockDag, blockHash: BlockHash, validator: Validator): Long =
    dag
      .dataLookup(blockHash)
      .parents
      .headOption
      .map(bh => dag.dataLookup(bh).weightMap.getOrElse(validator, 0L))
      .getOrElse(dag.dataLookup(blockHash).weightMap.getOrElse(validator, 0L))

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

  def deploys(b: BlockMessage): Seq[ProcessedDeploy] =
    b.body.fold(Seq.empty[ProcessedDeploy])(_.deploys)

  def tuplespace(b: BlockMessage): Option[ByteString] =
    for {
      bd <- b.body
      ps <- bd.postState
    } yield ps.tuplespace

  def bonds(b: BlockMessage): Seq[Bond] =
    (for {
      bd <- b.body
      ps <- bd.postState
    } yield ps.bonds).getOrElse(List.empty[Bond])

  def blockNumber(b: BlockMessage): Long =
    (for {
      bd <- b.body
      ps <- bd.postState
    } yield ps.blockNumber).getOrElse(0L)

  /*
   * Two blocks conflict if they both use the same deploy in different histories
   *
   * TODO: Update the logic of this function to make use of the trace logs and
   * say that two blocks don't conflict if they act on disjoint sets of channels
   */
  def conflicts[F[_]: Monad: BlockStore](
      b1: BlockMessage,
      b2: BlockMessage,
      genesis: BlockMessage,
      dag: BlockDag
  ): F[Boolean] =
    for {
      gca <- DagOperations.greatestCommonAncestorF[F](b1, b2, genesis, dag)
      result <- if (gca == b1 || gca == b2) {
                 //blocks which already exist in each other's chains do not conflict
                 false.pure[F]
               } else {
                 def getDeploys(b: BlockMessage) =
                   for {
                     bAncestors <- DagOperations
                                    .bfTraverseF[F, BlockMessage](List(b))(
                                      ProtoUtil.unsafeGetParents[F]
                                    )
                                    .takeWhile(_ != gca)
                                    .toList
                     deploys = bAncestors
                       .flatMap(b => {
                         b.body.map(_.deploys.flatMap(_.deploy)).getOrElse(List.empty[Deploy])
                       })
                       .toSet
                   } yield deploys
                 for {
                   b1Deploys <- getDeploys(b1)
                   b2Deploys <- getDeploys(b2)
                 } yield b1Deploys.intersect(b2Deploys).nonEmpty
               }
    } yield result

  def chooseNonConflicting[F[_]: Monad: BlockStore](
      blocks: Seq[BlockMessage],
      genesis: BlockMessage,
      dag: BlockDag
  ): F[Seq[BlockMessage]] = {
    def nonConflicting(b: BlockMessage): BlockMessage => F[Boolean] =
      conflicts[F](_, b, genesis, dag).map(b => !b)

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

  def toJustification(latestMessages: collection.Map[Validator, BlockMessage]): Seq[Justification] =
    latestMessages.toSeq.map {
      case (validator, block) =>
        Justification()
          .withValidator(validator)
          .withLatestBlockHash(block.blockHash)
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
      dag: BlockDag
  ): F[immutable.Map[Validator, BlockMessage]] =
    justifications.toList.foldM(Map.empty[Validator, BlockMessage]) {
      case (acc, Justification(validator, hash)) =>
        for {
          block <- ProtoUtil.unsafeGetBlock[F](hash)
        } yield acc.updated(validator, block)
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
      .withPostStateHash(protoHash(body.postState.get))
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

  def signBlock(
      block: BlockMessage,
      dag: BlockDag,
      pk: Array[Byte],
      sk: Array[Byte],
      sigAlgorithm: String,
      shardId: String
  ): BlockMessage = {

    val header = {
      //TODO refactor casper code to avoid the usage of Option fields in the block data structures
      // https://rchain.atlassian.net/browse/RHOL-572
      assert(block.header.isDefined, "A block without a header doesn't make sense")
      block.header.get
    }

    val sender = ByteString.copyFrom(pk)
    val seqNum = dag.latestMessages.get(sender).fold(0)(_.seqNum) + 1

    val blockHash = hashSignedBlock(header, sender, sigAlgorithm, seqNum, shardId, block.extraBytes)

    val sigAlgorithmBlock = block.withSigAlgorithm(sigAlgorithm)

    val sig = ByteString.copyFrom(sigAlgorithmBlock.signFunction(blockHash.toByteArray, sk))

    val signedBlock = sigAlgorithmBlock
      .withSender(sender)
      .withSig(sig)
      .withSeqNum(seqNum)
      .withBlockHash(blockHash)
      .withShardId(shardId)

    signedBlock
  }

  def hashString(b: BlockMessage): String = Base16.encode(b.blockHash.toByteArray)

  def stringToByteString(string: String): ByteString =
    ByteString.copyFrom(Base16.decode(string))

  def basicDeployData(id: Int): DeployData = {
    //TODO this should be removed once we assign the deploy with exact user
    Thread.sleep(1)
    val timestamp = System.currentTimeMillis()
    val term      = s"@${id}!($id)"

    DeployData()
      .withUser(ByteString.EMPTY)
      .withTimestamp(timestamp)
      .withTerm(term)
      .withPhloLimit(accounting.MAX_VALUE)
  }

  def basicDeploy(id: Int): Deploy = {
    val d    = basicDeployData(id)
    val term = InterpreterUtil.mkTerm(d.term).right.get
    Deploy(
      term = Some(term),
      raw = Some(d)
    )
  }

  def basicProcessedDeploy(id: Int): ProcessedDeploy = {
    val deploy = basicDeploy(id)
    ProcessedDeploy(
      deploy = Some(deploy),
      errored = false
    )
  }

  def sourceDeploy(source: String, timestamp: Long, phlos: PhloLimit): DeployData =
    DeployData(
      user = ByteString.EMPTY,
      timestamp = timestamp,
      term = source,
      phloLimit = Some(phlos)
    )

  def compiledSourceDeploy(
      source: CompiledRholangSource,
      timestamp: Long,
      phloLimit: PhloLimit
  ): Deploy =
    Deploy(
      term = Some(source.term),
      raw = Some(sourceDeploy(source.code, timestamp, phloLimit))
    )

  def termDeploy(term: Par, timestamp: Long, phloLimit: PhloLimit): Deploy =
    Deploy(
      term = Some(term),
      raw = Some(
        DeployData(
          user = ByteString.EMPTY,
          timestamp = timestamp,
          term = term.toProtoString,
          phloLimit = Some(phloLimit)
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
}
