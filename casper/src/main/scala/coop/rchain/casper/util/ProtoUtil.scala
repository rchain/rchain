package coop.rchain.casper.util

import cats.Monad
import cats.implicits._
import com.google.protobuf.{ByteString, Int32Value, StringValue}
import coop.rchain.casper.BlockDag
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

  @tailrec
  def getMainChain(internalMap: Map[BlockHash, BlockMessage],
                   estimate: BlockMessage,
                   acc: IndexedSeq[BlockMessage]): IndexedSeq[BlockMessage] = {
    val parentsHashes       = ProtoUtil.parents(estimate)
    val maybeMainParentHash = parentsHashes.headOption
    maybeMainParentHash flatMap internalMap.get match {
      case Some(newEstimate) =>
        getMainChain(internalMap, newEstimate, acc :+ estimate)
      case None => acc :+ estimate
    }
  }

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
      .withNewCodeHash(protoSeqHash(body.newCode))
      .withCommReductionsHash(protoSeqHash(body.commReductions))
      .withDeployCount(body.newCode.length)
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

  def hashUnsignedBlock(header: Header, justifications: Seq[Justification]) = {
    val items = header.toByteArray +: justifications.map(_.toByteArray)
    hashByteArrays(items: _*)
  }

  def hashSignedBlock(header: Header,
                      sender: ByteString,
                      sigAlgorithm: String,
                      seqNum: Int,
                      shardId: String,
                      extraBytes: ByteString) =
    hashByteArrays(
      header.toByteArray,
      sender.toByteArray,
      StringValue.of(sigAlgorithm).toByteArray,
      Int32Value.of(seqNum).toByteArray,
      StringValue.of(shardId).toByteArray,
      extraBytes.toByteArray
    )

  def signBlock(block: BlockMessage,
                dag: BlockDag,
                pk: Array[Byte],
                sk: Array[Byte],
                sigAlgorithm: String,
                signFunction: (Array[Byte], Array[Byte]) => Array[Byte],
                shardId: String): BlockMessage = {

    val header = {
      //TODO refactor casper code to avoid the usage of Option fields in the block datastructures
      // https://rchain.atlassian.net/browse/RHOL-572
      assert(block.header.isDefined, "A block without a header doesn't make sense")
      block.header.get
    }

    val sender = ByteString.copyFrom(pk)
    val seqNum = dag.currentSeqNum.getOrElse(sender, -1) + 1

    val blockHash = hashSignedBlock(header, sender, sigAlgorithm, seqNum, shardId, block.extraBytes)

    val sig = ByteString.copyFrom(signFunction(blockHash.toByteArray, sk))

    val signedBlock = block
      .withSender(sender)
      .withSig(sig)
      .withSeqNum(seqNum)
      .withSigAlgorithm(sigAlgorithm)
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
  }

  def basicDeploy(id: Int): Deploy = {
    val d    = basicDeployData(id)
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

  def sourceDeploy(source: String): DeployData = {
    //TODO this should be removed once we assign the deploy with exact user
    Thread.sleep(1)
    val timestamp = System.currentTimeMillis()
    DeployData(user = ByteString.EMPTY, timestamp = timestamp, term = source)
  }

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
