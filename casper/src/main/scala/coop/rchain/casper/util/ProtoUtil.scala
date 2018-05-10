package coop.rchain.casper.util

import com.google.protobuf.ByteString

import coop.rchain.casper.BlockDag
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Sha256

import scala.annotation.tailrec

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

  def parents(b: BlockMessage): Seq[ByteString] =
    b.header.map(_.parentsHashList).getOrElse(List.empty[ByteString])

  //Two blocks conflict if they both use the same deploy in different histories
  def conflicts(b1: BlockMessage,
                b2: BlockMessage,
                genesis: BlockMessage,
                dag: BlockDag): Boolean = {
    val gca = DagOperations.greatestCommonAncestor(b1, b2, genesis, dag)
    if (gca == b1 || gca == b2) {
      //blocks which already in each other's chains do not conflict
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

  def justificationProto(
      latestMessages: collection.Map[ByteString, ByteString]): Seq[Justification] =
    latestMessages.toSeq.map {
      case (validator, block) =>
        Justification()
          .withValidator(validator)
          .withLatestBlockHash(block)
    }

  def protoHash[A <: { def toByteArray: Array[Byte] }](proto: A): ByteString =
    ByteString.copyFrom(Sha256.hash(proto.toByteArray))

  def protoSeqHash[A <: { def toByteArray: Array[Byte] }](protoSeq: Seq[A]): ByteString = {
    val bytes = protoSeq.foldLeft(Array.empty[Byte]) {
      case (acc, proto) => acc ++ proto.toByteArray
    }
    ByteString.copyFrom(Sha256.hash(bytes))
  }

  def blockHeader(body: Body, parentHashes: Seq[ByteString]): Header =
    Header()
      .withParentsHashList(parentHashes)
      .withPostStateHash(protoHash(body.postState.get))
      .withNewCodeHash(protoSeqHash(body.newCode))
      .withCommReductionsHash(protoSeqHash(body.commReductions))

  //TODO: add signature
  def blockProto(body: Body,
                 header: Header,
                 justifications: Seq[Justification],
                 sender: ByteString): BlockMessage =
    BlockMessage()
      .withBlockHash(protoHash(header))
      .withHeader(header)
      .withBody(body)
      .withJustifications(justifications)
      .withSender(sender)

  def genesisBlock: BlockMessage = {
    val bonds = (1 to 10).map(i => {
      val validator = ByteString.copyFrom(Array(i.toByte))
      val stake     = i
      Bond(validator, stake)
    })
    val state = RChainState()
      .withBlockNumber(0)
      .withBonds(bonds)
    val body = Body()
      .withPostState(state)
    val header = blockHeader(body, List.empty[ByteString])

    blockProto(body, header, List.empty[Justification], ByteString.copyFrom(Array.empty[Byte]))
  }

  def hashString(b: BlockMessage): String = Base16.encode(b.blockHash.toByteArray)

  def basicDeploy(id: Int): Deploy = {
    val nonce = scala.util.Random.nextInt(10000)
    val term  = InterpreterUtil.mkTerm(s"@${id}!($id)").right.get

    Deploy()
      .withUser(ByteString.EMPTY)
      .withNonce(nonce)
      .withTerm(term)
  }
}
