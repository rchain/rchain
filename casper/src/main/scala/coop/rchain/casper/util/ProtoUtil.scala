package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper.BlockDag
import coop.rchain.casper.EquivocationRecord.SequenceNumber
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.models.Par

import scala.annotation.tailrec
import scala.collection.immutable

object ProtoUtil {
  /*
   * c is in the blockchain of b iff c == b or c is in the blockchain of the main parent of b
   */
  // TODO: Move into BlockDAG and remove corresponding param once that is moved over from simulator
  @tailrec
  def isInMainChain(blocks: collection.Map[ByteString, BlockMessage],
                    candidate: BlockMessage,
                    target: BlockMessage): Boolean =
    if (candidate == target) {
      true
    } else {
      (for {
        hdr        <- target.header
        parentHash <- hdr.parentsHashList.headOption
        mainParent <- blocks.get(parentHash)
      } yield mainParent) match {
        case Some(parent) => isInMainChain(blocks, candidate, parent)
        case None         => false
      }
    }

  @tailrec
  def getMainChain(blockDag: BlockDag,
                   estimate: BlockMessage,
                   acc: IndexedSeq[BlockMessage]): IndexedSeq[BlockMessage] = {
    val parentsHashes       = ProtoUtil.parents(estimate)
    val maybeMainParentHash = parentsHashes.headOption
    maybeMainParentHash.flatMap(blockDag.blockLookup.get) match {
      case Some(newEstimate) =>
        getMainChain(blockDag, newEstimate, acc :+ estimate)
      case None => acc :+ estimate
    }
  }

  @tailrec
  def findJustificationParentWithSeqNum(b: BlockMessage,
                                        blockLookup: collection.Map[BlockHash, BlockMessage],
                                        seqNum: SequenceNumber): Option[BlockMessage] =
    if (b.seqNum == seqNum) {
      Some(b)
    } else {
      val creatorJustificationHash = b.justifications.find {
        case Justification(validator, _) => validator == b.sender
      }
      creatorJustificationHash match {
        case Some(Justification(_, blockHash)) =>
          val creatorJustification = blockLookup.get(blockHash)
          creatorJustification match {
            case Some(block) => findJustificationParentWithSeqNum(block, blockLookup, seqNum)
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

  def mainParent(blocks: collection.Map[ByteString, BlockMessage],
                 blockMessage: BlockMessage): Option[BlockMessage] =
    for {
      hdr        <- blockMessage.header
      parentHash <- hdr.parentsHashList.headOption
      mainParent <- blocks.get(parentHash)
    } yield mainParent

  def weightFromValidator(b: BlockMessage,
                          validator: ByteString,
                          blocks: collection.Map[ByteString, BlockMessage]): Int =
    mainParent(blocks, b)
      .map(weightMap(_).getOrElse(validator, 0))
      .getOrElse(weightMap(b).getOrElse(validator, 0)) //no parents means genesis -- use itself

  def weightFromSender(b: BlockMessage, blocks: collection.Map[ByteString, BlockMessage]): Int =
    weightFromValidator(b, b.sender, blocks)

  def parents(b: BlockMessage): Seq[ByteString] =
    b.header.map(_.parentsHashList).getOrElse(List.empty[ByteString])

  def deploys(b: BlockMessage): Seq[Deploy] =
    b.body.map(_.newCode).getOrElse(List.empty[Deploy])

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
                dag: BlockDag): Boolean = {
    val gca = DagOperations.greatestCommonAncestor(b1, b2, genesis, dag)
    if (gca == b1 || gca == b2) {
      //blocks which already exist in each other's chains do not conflict
      false
    } else {
      def getDeploys(b: BlockMessage) =
        DagOperations
          .bfTraverse[BlockMessage](Some(b))(parents(_).iterator.map(dag.blockLookup))
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
                           dag: BlockDag): Seq[BlockMessage] =
    blocks
      .foldLeft(List.empty[BlockMessage]) {
        case (acc, b) =>
          if (acc.forall(!conflicts(_, b, genesis, dag))) {
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

  /**
    * Interprets the byte array as a large positive number, adds the
    * given integer by usual addition, then turns the result back into
    * a byte array.
    * @param n Byte array to interpret as large positive number
    * @param m Integer to add to `n`
    * @return Result of the addition, changes back to a byte array.
    */
  def add(n: Array[Byte], m: Int): Array[Byte] = {
    val num = BigInt(Base16.encode(n), 16)
    val sum = num + m

    Base16.decode(sum.toString(16))
  }

  def stringToByteString(string: String): ByteString =
    ByteString.copyFrom(Base16.decode(string))

  def basicDeployString(id: Int): DeployString = {
    val timestamp = System.currentTimeMillis()
    val term      = s"@${id}!($id)"

    DeployString()
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

  def termDeploy(term: Par): Deploy = {
    val timestamp = System.currentTimeMillis()
    Deploy(
      term = Some(term),
      raw = Some(
        DeployString(user = ByteString.EMPTY, timestamp = timestamp, term = term.toProtoString))
    )
  }
}
