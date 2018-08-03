package coop.rchain.casper.util

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.{BlockDag, PrettyPrinter}
import coop.rchain.casper.EquivocationRecord.SequenceNumber
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.models.{PCost, Par}

import scala.annotation.tailrec
import scala.collection.immutable

object ProtoUtil {
  /*
   * c is in the blockchain of b iff c == b or c is in the blockchain of the main parent of b
   */
  // TODO: Move into BlockDAG and remove corresponding param once that is moved over from simulator
  @tailrec
  def isInMainChain(internalMap: Map[BlockHash, BlockMessage],
                    candidate: BlockMessage,
                    target: BlockMessage): Boolean =
    if (candidate == target) {
      true
    } else {
      (for {
        hdr        <- target.header
        parentHash <- hdr.parentsHashList.headOption
        mainParent <- internalMap.get(parentHash)
      } yield mainParent) match {
        case Some(parent) => isInMainChain(internalMap, candidate, parent)
        case None         => false
      }
    }

  def getMainChain[F[_]: Monad: BlockStore](
      estimate: BlockMessage,
      acc: IndexedSeq[BlockMessage]): F[IndexedSeq[BlockMessage]] = {
    val parentsHashes       = ProtoUtil.parents(estimate)
    val maybeMainParentHash = parentsHashes.headOption
    for {
      mainChain <- maybeMainParentHash match {
                    case Some(mainParentHash) =>
                      for {
                        updatedEstimate <- getBlock[F](mainParentHash)
                        mainChain       <- getMainChain[F](updatedEstimate, acc :+ estimate)
                      } yield mainChain
                    case None => (acc :+ estimate).pure[F]
                  }
    } yield mainChain
  }

  def getBlock[F[_]: Monad: BlockStore](hash: BlockHash): F[BlockMessage] =
    for {
      maybeBlock <- BlockStore[F].get(hash)
      block = maybeBlock match {
        case Some(block) =>
          block
        case None =>
          throw new Error(s"BlockStore is missing hash ${PrettyPrinter.buildString(hash)}")
      }
    } yield block

  @tailrec
  def findJustificationParentWithSeqNum(b: BlockMessage,
                                        internalMap: Map[BlockHash, BlockMessage],
                                        seqNum: SequenceNumber): Option[BlockMessage] =
    if (b.seqNum == seqNum) {
      Some(b)
    } else {
      val creatorJustificationHash = b.justifications.find {
        case Justification(validator, _) => validator == b.sender
      }
      creatorJustificationHash match {
        case Some(Justification(_, blockHash)) =>
          val creatorJustification = internalMap.get(blockHash)
          creatorJustification match {
            case Some(block) => findJustificationParentWithSeqNum(block, internalMap, seqNum)
            case None        => None
          }
        case None => None
      }
    }

  def weightMap(blockMessage: BlockMessage): Map[ByteString, Int] =
    blockMessage.body match {
      case Some(block) =>
        block.postState match {
          case Some(state) => weightMap(state)
          case None        => Map.empty[ByteString, Int]
        }
      case None => Map.empty[ByteString, Int]
    }

  private def weightMap(state: RChainState): Map[ByteString, Int] =
    state.bonds.map {
      case Bond(validator, stake) => validator -> stake
    }.toMap

  def weightMapTotal(weights: Map[ByteString, Int]): Int =
    weights.values.sum

  def minTotalValidatorWeight(blockMessage: BlockMessage, maxCliqueMinSize: Int): Int = {
    val sortedWeights = weightMap(blockMessage).values.toList.sorted
    sortedWeights.take(maxCliqueMinSize).sum
  }

  def mainParent(internalMap: Map[BlockHash, BlockMessage],
                 blockMessage: BlockMessage): Option[BlockMessage] =
    for {
      hdr        <- blockMessage.header
      parentHash <- hdr.parentsHashList.headOption
      mainParent <- internalMap.get(parentHash)
    } yield mainParent

  def weightFromValidator(b: BlockMessage,
                          validator: ByteString,
                          internalMap: Map[BlockHash, BlockMessage]): Int =
    mainParent(internalMap, b)
      .map(weightMap(_).getOrElse(validator, 0))
      .getOrElse(weightMap(b).getOrElse(validator, 0)) //no parents means genesis -- use itself

  def weightFromSender(b: BlockMessage, internalMap: Map[BlockHash, BlockMessage]): Int =
    weightFromValidator(b, b.sender, internalMap)

  def parents(b: BlockMessage): Seq[ByteString] =
    b.header.map(_.parentsHashList).getOrElse(List.empty[ByteString])

  def deploys(b: BlockMessage): Seq[Deploy] =
    b.body.map(_.newCode.flatMap(_.deploy)).getOrElse(List.empty[Deploy])

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

  //Two blocks conflict if they both use the same deploy in different histories
  def conflicts(b1: BlockMessage,
                b2: BlockMessage,
                genesis: BlockMessage,
                dag: BlockDag,
                internalMap: Map[BlockHash, BlockMessage]): Boolean = {
    val gca = DagOperations.greatestCommonAncestor(b1, b2, genesis, dag, internalMap)
    if (gca == b1 || gca == b2) {
      //blocks which already exist in each other's chains do not conflict
      false
    } else {
      def getDeploys(b: BlockMessage) =
        DagOperations
          .bfTraverse[BlockMessage](Some(b))(parents(_).iterator.map(internalMap(_)))
          .takeWhile(_ != gca)
          .flatMap(b => {
            b.body.map(_.newCode).getOrElse(List.empty[Deploy])
          })
          .toSet

      getDeploys(b1).intersect(getDeploys(b2)).nonEmpty
    }
  }

  def chooseNonConflicting(blocks: Seq[BlockMessage],
                           genesis: BlockMessage,
                           dag: BlockDag,
                           internalMap: Map[BlockHash, BlockMessage]): Seq[BlockMessage] =
    blocks
      .foldLeft(List.empty[BlockMessage]) {
        case (acc, b) =>
          if (acc.forall(!conflicts(_, b, genesis, dag, internalMap))) {
            b :: acc
          } else {
            acc
          }
      }
      .reverse

  def toJustification(latestMessages: collection.Map[Validator, BlockHash]): Seq[Justification] =
    latestMessages.toSeq.map {
      case (validator, block) =>
        Justification()
          .withValidator(validator)
          .withLatestBlockHash(block)
    }

  def toLatestMessages(justifications: Seq[Justification]): immutable.Map[Validator, BlockHash] =
    justifications.foldLeft(Map.empty[Validator, BlockHash]) {
      case (acc, Justification(validator, block)) =>
        acc.updated(validator, block)
    }

  def protoHash[A <: { def toByteArray: Array[Byte] }](proto: A): ByteString =
    ByteString.copyFrom(Blake2b256.hash(proto.toByteArray))

  def protoSeqHash[A <: { def toByteArray: Array[Byte] }](protoSeq: Seq[A]): ByteString = {
    val bytes = protoSeq.foldLeft(Array.empty[Byte]) {
      case (acc, proto) => acc ++ proto.toByteArray
    }
    ByteString.copyFrom(Blake2b256.hash(bytes))
  }

  def blockHeader(body: Body,
                  parentHashes: Seq[ByteString],
                  version: Long,
                  timestamp: Long): Header =
    Header()
      .withParentsHashList(parentHashes)
      .withPostStateHash(protoHash(body.postState.get))
      .withNewCodeHash(protoSeqHash(body.newCode))
      .withCommReductionsHash(protoSeqHash(body.commReductions))
      .withDeployCount(body.newCode.length)
      .withVersion(version)
      .withTimestamp(timestamp)

  def unsignedBlockProto(body: Body,
                         header: Header,
                         justifications: Seq[Justification]): BlockMessage =
    BlockMessage()
      .withBlockHash(protoHash(header))
      .withHeader(header)
      .withBody(body)
      .withJustifications(justifications)

  def signBlock(block: BlockMessage,
                dag: BlockDag,
                pk: Array[Byte],
                sk: Array[Byte],
                sigAlgorithm: String,
                signFunction: (Array[Byte], Array[Byte]) => Array[Byte]): BlockMessage = {
    val justificationHash = ProtoUtil.protoSeqHash(block.justifications)
    val sigData           = Blake2b256.hash(justificationHash.toByteArray ++ block.blockHash.toByteArray)
    val sender            = ByteString.copyFrom(pk)
    val sig               = ByteString.copyFrom(signFunction(sigData, sk))
    val currSeqNum        = dag.currentSeqNum.getOrElse(sender, -1)
    val signedBlock = block
      .withSender(sender)
      .withSig(sig)
      .withSeqNum(currSeqNum + 1)
      .withSigAlgorithm(sigAlgorithm)

    signedBlock
  }

  def hashString(b: BlockMessage): String = Base16.encode(b.blockHash.toByteArray)

  def stringToByteString(string: String): ByteString =
    ByteString.copyFrom(Base16.decode(string))

  def basicDeployString(id: Int): DeployData = {
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
    val d    = basicDeployString(id)
    val term = InterpreterUtil.mkTerm(d.term).right.get
    Deploy(
      term = Some(term),
      raw = Some(d)
    )
  }

  def basicDeployCost(id: Int): DeployCost =
    DeployCost()
      .withDeploy(basicDeploy(id))
      .withCost(PCost(1L, 1))

  def termDeploy(term: Par): Deploy = {
    //TODO this should be removed once we assign the deploy with exact user
    Thread.sleep(1)
    val timestamp = System.currentTimeMillis()
    Deploy(
      term = Some(term),
      raw =
        Some(DeployData(user = ByteString.EMPTY, timestamp = timestamp, term = term.toProtoString))
    )
  }
}
