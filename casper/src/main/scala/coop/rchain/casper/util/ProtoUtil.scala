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

import scala.collection.immutable

object ProtoUtil {
  /*
   * c is in the blockchain of b iff c == b or c is in the blockchain of the main parent of b
   */
  // TODO: Move into BlockDAG and remove corresponding param once that is moved over from simulator
  def isInMainChain[F[_]: Monad: BlockStore](candidate: BlockMessage.Safe,
                                             target: BlockMessage.Safe): F[Boolean] =
    if (candidate == target) {
      true.pure[F]
    } else {
      val maybeMainParentHash = ProtoUtil.parentHashes(target).headOption
      maybeMainParentHash match {
        case Some(mainParentHash) =>
          for {
            mainParent <- BlockStore[F].get(mainParentHash)
            isInMainChain <- mainParent match {
                              case Some(parent) => isInMainChain[F](candidate, parent)
                              case None         => false.pure[F]
                            }
          } yield isInMainChain
        case None => false.pure[F]
      }
    }

  def getMainChain[F[_]: Monad: BlockStore](
      estimate: BlockMessage.Safe,
      acc: IndexedSeq[BlockMessage.Safe]): F[IndexedSeq[BlockMessage.Safe]] = {
    val parentsHashes       = ProtoUtil.parentHashes(estimate)
    val maybeMainParentHash = parentsHashes.headOption
    for {
      mainChain <- maybeMainParentHash match {
                    case Some(mainParentHash) =>
                      for {
                        updatedEstimate <- unsafeGetBlock[F](mainParentHash)
                        mainChain       <- getMainChain[F](updatedEstimate, acc :+ estimate)
                      } yield mainChain
                    case None => (acc :+ estimate).pure[F]
                  }
    } yield mainChain
  }

  def unsafeGetBlock[F[_]: Monad: BlockStore](hash: BlockHash): F[BlockMessage.Safe] =
    for {
      maybeBlock <- BlockStore[F].get(hash)
      block = maybeBlock match {
        case Some(b) => b
        case None =>
          throw new Exception(s"BlockStore is missing hash ${PrettyPrinter.buildString(hash)}")
      }
    } yield block

  def creatorJustification(block: BlockMessage.Safe): Option[Justification] =
    block.justifications
      .find {
        case Justification(validator: Validator, _) =>
          validator == block.sender
      }

  def findCreatorJustificationAncestorWithSeqNum[F[_]: Monad: BlockStore](
      b: BlockMessage.Safe,
      seqNum: SequenceNumber): F[Option[BlockMessage.Safe]] =
    if (b.seqNum == seqNum) {
      Option[BlockMessage.Safe](b).pure[F]
    } else {
      DagOperations
        .bfTraverseF(List(b)) { block =>
          getCreatorJustificationAsList[F](block, block.sender)
        }
        .find(_.seqNum == seqNum)
    }

  def getCreatorJustificationAsList[F[_]: Monad: BlockStore](
      block: BlockMessage.Safe,
      validator: Validator,
      goalFunc: BlockMessage.Safe => Boolean = _ => false): F[List[BlockMessage.Safe]] = {
    val maybeCreatorJustificationHash =
      block.justifications.find(_.validator == validator)
    maybeCreatorJustificationHash match {
      case Some(creatorJustificationHash) =>
        for {
          maybeCreatorJustification <- BlockStore[F].get(creatorJustificationHash.latestBlockHash)
          maybeCreatorJustificationAsList = maybeCreatorJustification match {
            case Some(creatorJustification) =>
              if (goalFunc(creatorJustification)) {
                List.empty[BlockMessage.Safe]
              } else {
                List(creatorJustification)
              }
            case None =>
              List.empty[BlockMessage.Safe]
          }
        } yield maybeCreatorJustificationAsList
      case None => List.empty[BlockMessage.Safe].pure[F]
    }
  }

  def weightMap(blockMessage: BlockMessage.Safe): Map[ByteString, Int] =
    weightMap(blockMessage.body.postState.underlying)

  private def weightMap(state: RChainState): Map[ByteString, Int] =
    state.bonds.map {
      case Bond(validator, stake) => validator -> stake
    }.toMap

  def weightMapTotal(weights: Map[ByteString, Int]): Int =
    weights.values.sum

  def minTotalValidatorWeight(blockMessage: BlockMessage.Safe, maxCliqueMinSize: Int): Int = {
    val sortedWeights = weightMap(blockMessage).values.toList.sorted
    sortedWeights.take(maxCliqueMinSize).sum
  }

  def mainParent[F[_]: Monad: BlockStore](
      blockMessage: BlockMessage.Safe): F[Option[BlockMessage.Safe]] = {
    val maybeParentHash = blockMessage.header.parentsHashList.headOption
    maybeParentHash match {
      case Some(parentHash) => BlockStore[F].get(parentHash)
      case None             => none[BlockMessage.Safe].pure[F]
    }
  }

  def weightFromValidator[F[_]: Monad: BlockStore](b: BlockMessage.Safe,
                                                   validator: ByteString): F[Int] =
    for {
      maybeMainParent <- mainParent[F](b)
      weightFromValidator = maybeMainParent
        .map(weightMap(_).getOrElse(validator, 0))
        .getOrElse(weightMap(b).getOrElse(validator, 0)) //no parents means genesis -- use itself
    } yield weightFromValidator

  def weightFromSender[F[_]: Monad: BlockStore](b: BlockMessage.Safe): F[Int] =
    weightFromValidator[F](b, b.sender)

  def parentHashes(b: BlockMessage.Safe): Seq[ByteString] =
    b.header.parentsHashList

  def unsafeGetParents[F[_]: Monad: BlockStore](b: BlockMessage.Safe): F[List[BlockMessage.Safe]] =
    b.header.parentsHashList.toList.traverse { parentHash =>
      ProtoUtil.unsafeGetBlock[F](parentHash)
    }

  def deploys(b: BlockMessage.Safe): Seq[ProcessedDeploy] =
    b.body.deploys

  def tuplespace(b: BlockMessage.Safe): ByteString =
    b.body.postState.tuplespace

  def bonds(b: BlockMessage.Safe): Seq[Bond] =
    b.body.postState.bonds

  def blockNumber(b: BlockMessage.Safe): Long =
    b.body.postState.blockNumber

  /*
   * Two blocks conflict if they both use the same deploy in different histories
   *
   * TODO: Update the logic of this function to make use of the trace logs and
   * say that two blocks don't conflict if they act on disjoint sets of channels
   */
  def conflicts[F[_]: Monad: BlockStore](b1: BlockMessage.Safe,
                                         b2: BlockMessage.Safe,
                                         genesis: BlockMessage.Safe,
                                         dag: BlockDag): F[Boolean] =
    for {
      gca <- DagOperations.greatestCommonAncestorF[F](b1, b2, genesis, dag)
      result <- if (gca == b1 || gca == b2) {
                 //blocks which already exist in each other's chains do not conflict
                 false.pure[F]
               } else {
                 def getDeploys(b: BlockMessage.Safe) =
                   for {
                     bAncestors <- DagOperations
                                    .bfTraverseF[F, BlockMessage.Safe](List(b))(
                                      ProtoUtil.unsafeGetParents[F])
                                    .toList
                     deploys = bAncestors
                       .takeWhile(_ != gca)
                       .flatMap(b => {
                         b.body.deploys.flatMap(_.deploy)
                       })
                       .toSet
                   } yield deploys
                 for {
                   b1Deploys <- getDeploys(b1)
                   b2Deploys <- getDeploys(b2)
                 } yield b1Deploys.intersect(b2Deploys).nonEmpty
               }
    } yield result

  def chooseNonConflicting[F[_]: Monad: BlockStore](blocks: Seq[BlockMessage.Safe],
                                                    genesis: BlockMessage.Safe,
                                                    dag: BlockDag): F[Seq[BlockMessage.Safe]] = {
    def nonConflicting(b: BlockMessage.Safe): BlockMessage.Safe => F[Boolean] =
      conflicts[F](_, b, genesis, dag).map(b => !b)

    blocks.toList
      .foldM(List.empty[BlockMessage.Safe]) {
        case (acc, b) =>
          Monad[F].ifM(acc.forallM(nonConflicting(b)))(
            (b :: acc).pure[F],
            acc.pure[F]
          )
      }
      .map(_.reverse)
  }

  def toJustification(
      latestMessages: collection.Map[Validator, BlockMessage.Safe]): Seq[Justification] =
    latestMessages.toSeq.map {
      case (validator, block) =>
        Justification()
          .withValidator(validator)
          .withLatestBlockHash(block.blockHash)
    }

  def toLatestMessageHashes(
      justifications: Seq[Justification]): immutable.Map[Validator, BlockHash] =
    justifications.foldLeft(Map.empty[Validator, BlockHash]) {
      case (acc, Justification(validator, block)) =>
        acc.updated(validator, block)
    }

  def toLatestMessage[F[_]: Monad: BlockStore](
      justifications: Seq[Justification],
      dag: BlockDag): F[immutable.Map[Validator, BlockMessage.Safe]] =
    justifications.toList.foldM(Map.empty[Validator, BlockMessage.Safe]) {
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

  def blockHeader(body: Body,
                  parentHashes: Seq[ByteString],
                  version: Long,
                  timestamp: Long): Header =
    Header()
      .withParentsHashList(parentHashes)
      .withPostStateHash(protoHash(body.postState.get))
      .withDeploysHash(protoSeqHash(body.deploys))
      .withDeployCount(body.deploys.size)
      .withVersion(version)
      .withTimestamp(timestamp)

  def unsignedBlockProto(body: Body,
                         header: Header,
                         justifications: Seq[Justification],
                         shardId: String): BlockMessage = {
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

  def hashSignedBlock(header: Header.Safe,
                      sender: ByteString,
                      sigAlgorithm: String,
                      seqNum: Int,
                      shardId: String,
                      extraBytes: ByteString): BlockHash =
    hashByteArrays(
      header.toByteArray,
      sender.toByteArray,
      StringValue.of(sigAlgorithm).toByteArray,
      Int32Value.of(seqNum).toByteArray,
      StringValue.of(shardId).toByteArray,
      extraBytes.toByteArray
    )

  def signBlock(blockSafe: BlockMessage.Safe,
                dag: BlockDag,
                pk: Array[Byte],
                sk: Array[Byte],
                sigAlgorithm: String,
                shardId: String): BlockMessage.Safe = {

    val header = blockSafe.header
    val block  = blockSafe.underlying

    val sender = ByteString.copyFrom(pk)
    val seqNum = dag.currentSeqNum.getOrElse(sender, 0) + 1

    val blockHash = hashSignedBlock(header, sender, sigAlgorithm, seqNum, shardId, block.extraBytes)

    val sigAlgorithmBlock = block.withSigAlgorithm(sigAlgorithm)

    val sig = ByteString.copyFrom(sigAlgorithmBlock.signFunction(blockHash.toByteArray, sk))

    val signedBlock = sigAlgorithmBlock
      .withSender(sender)
      .withSig(sig)
      .withSeqNum(seqNum)
      .withBlockHash(blockHash)
      .withShardId(shardId)

    BlockMessage.Safe
      .create(signedBlock)
      .getOrElse(sys.error("A valid block became malformed after signing"))
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

  def sourceDeploy(source: String, timestamp: Long): DeployData =
    DeployData(user = ByteString.EMPTY, timestamp = timestamp, term = source)

  def compiledSourceDeploy(source: CompiledRholangSource, timestamp: Long): Deploy =
    Deploy(
      term = Some(source.term),
      raw = Some(sourceDeploy(source.code, timestamp))
    )

  def termDeploy(term: Par, timestamp: Long): Deploy =
    Deploy(
      term = Some(term),
      raw =
        Some(DeployData(user = ByteString.EMPTY, timestamp = timestamp, term = term.toProtoString))
    )

  def termDeployNow(term: Par): Deploy = termDeploy(term, System.currentTimeMillis())

  /**
    * Strip a deploy down to the fields we are using to seed the Deterministic name generator.
    * The fields stripped are the term and anything that depends on the term (Currently only the sig)
    */
  def stripDeployData(d: DeployData): DeployData =
    d.withTerm("").withSig(ByteString.EMPTY)
}
