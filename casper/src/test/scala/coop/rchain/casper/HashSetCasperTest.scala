package coop.rchain.casper

import cats.Id
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.comm.transport.CommMessages.packet
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

class HashSetCasperTest extends FlatSpec with Matchers {

  import HashSetCasperTest.blockTuplespaceContents

  val (otherSk, _)                = Ed25519.newKeyPair
  val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  val bonds                       = validators.zipWithIndex.map { case (v, i) => v -> (2 * i + 1) }.toMap
  val genesis                     = Genesis.withoutContracts(bonds = bonds, version = 0L, timestamp = 0L)

  //put a new casper instance at the start of each
  //test since we cannot reset it
  "HashSetCasper" should "accept deploys" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    import node._

    val deploy = ProtoUtil.basicDeploy(0)
    MultiParentCasper[Id].deploy(deploy)

    logEff.infos.size should be(1)
    logEff.infos.head.contains("CASPER: Received Deploy") should be(true)
  }

  it should "create blocks based on deploys" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    import node._

    val deploy = ProtoUtil.basicDeploy(0)
    MultiParentCasper[Id].deploy(deploy)

    val Some(block) = MultiParentCasper[Id].createBlock
    val parents     = ProtoUtil.parents(block)
    val deploys     = block.body.get.newCode
    val storage     = blockTuplespaceContents(block)

    parents.size should be(1)
    parents.head should be(genesis.blockHash)
    deploys.size should be(1)
    deploys.head should be(deploy)
    storage.contains("@{0}!(0)") should be(true)
  }

  it should "accept signed blocks" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    import node._

    val deploy = ProtoUtil.basicDeploy(0)
    MultiParentCasper[Id].deploy(deploy)

    val Some(signedBlock) = MultiParentCasper[Id].createBlock

    MultiParentCasper[Id].addBlock(signedBlock)

    val logMessages = List(
      "CASPER: Received Deploy",
      "CASPER: Beginning send of Block #1",
      "CASPER: Sent",
      "CASPER: Added",
      "CASPER: New fork-choice tip is block"
    )

    logEff.warns.isEmpty should be(true)
    logEff.infos.zip(logMessages).forall { case (a, b) => a.startsWith(b) } should be(true)
    MultiParentCasper[Id].estimator should be(IndexedSeq(signedBlock))
  }

  it should "be able to create a chain of blocks from different deploys" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    import node._

    val deploys = Vector(
      "contract @\"add\"(@x, @y, ret) = { ret!(x + y) }",
      "new unforgable in { @\"add\"!(5, 7, *unforgable) }"
    ).map(s => ProtoUtil.termDeploy(InterpreterUtil.mkTerm(s).right.get))

    val Some(signedBlock1) = MultiParentCasper[Id].deploy(deploys.head) *> MultiParentCasper[Id].createBlock
    MultiParentCasper[Id].addBlock(signedBlock1)

    val Some(signedBlock2) = MultiParentCasper[Id].deploy(deploys(1)) *> MultiParentCasper[Id].createBlock
    MultiParentCasper[Id].addBlock(signedBlock2)
    val storage = blockTuplespaceContents(signedBlock2)

    logEff.warns should be(Nil)
    ProtoUtil.parents(signedBlock2) should be(Seq(signedBlock1.blockHash))
    MultiParentCasper[Id].estimator should be(IndexedSeq(signedBlock2))
    storage.contains("!(12)") should be(true)
  }

  it should "reject unsigned blocks" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)
    import node._

    val Some(block) = MultiParentCasper[Id].deploy(ProtoUtil.basicDeploy(0)) *> MultiParentCasper[
      Id].createBlock
    val invalidBlock = block.withSig(ByteString.EMPTY)

    MultiParentCasper[Id].addBlock(invalidBlock)

    logEff.warns.head.contains("CASPER: Ignoring block") should be(true)
  }

  it should "reject blocks not from bonded validators" in {
    val node = HashSetCasperTestNode.standalone(genesis, otherSk)
    import node._

    val Some(signedBlock) = MultiParentCasper[Id].deploy(ProtoUtil.basicDeploy(0)) *> MultiParentCasper[
      Id].createBlock

    MultiParentCasper[Id].addBlock(signedBlock)

    logEff.warns.head.contains("CASPER: Ignoring block") should be(true)
  }

  it should "propose blocks it adds to peers" in {
    val nodes  = HashSetCasperTestNode.network(validatorKeys.take(2), genesis)
    val deploy = ProtoUtil.basicDeploy(0)

    val Some(signedBlock) = nodes(0).casperEff.deploy(deploy) *> nodes(0).casperEff.createBlock

    nodes(0).casperEff.addBlock(signedBlock)
    nodes(1).receive()

    val received = nodes(1).casperEff.contains(signedBlock)

    received should be(true)
  }

  it should "ask peers for blocks it is missing" in {
    val nodes   = HashSetCasperTestNode.network(validatorKeys.take(3), genesis)
    val deploys = (0 until 3).map(i => ProtoUtil.basicDeploy(i))

    val Some(signedBlock1) = nodes(0).casperEff.deploy(deploys(0)) *> nodes(0).casperEff.createBlock

    nodes(0).casperEff.addBlock(signedBlock1)
    nodes(1).receive()
    nodes(2).transportLayerEff.msgQueues(nodes(2).local).clear //nodes(2) misses this block

    val Some(signedBlock2) = nodes(0).casperEff.deploy(deploys(1)) *> nodes(0).casperEff.createBlock

    nodes(0).casperEff.addBlock(signedBlock2)
    nodes(1).receive() //receives block2
    nodes(2).receive() //receives block2; asks for block1
    nodes(1).receive() //receives request for block1; sends block1
    nodes(2).receive() //receives block1; adds both block1 and block2

    nodes(2).casperEff.contains(signedBlock1) should be(true)
    nodes(2).casperEff.contains(signedBlock2) should be(true)

    nodes(2).logEff.infos
      .count(_ startsWith "CASPER: Beginning request of missing block") should be(1)
    nodes(1).logEff.infos.count(s =>
      (s startsWith "CASPER: Received request for block") && (s endsWith "Response sent.")) should be(
      1)
  }

  it should "ignore adding equivocation blocks" in {
    val node = HashSetCasperTestNode.standalone(genesis, validatorKeys.head)

    // Creates a pair that constitutes equivocation blocks
    val Some(signedBlock1)      = node.casperEff.deploy(ProtoUtil.basicDeploy(0)) *> node.casperEff.createBlock
    val Some(signedBlock1Prime) = node.casperEff.deploy(ProtoUtil.basicDeploy(1)) *> node.casperEff.createBlock

    node.casperEff.addBlock(signedBlock1)
    node.casperEff.addBlock(signedBlock1Prime)

    node.casperEff.contains(signedBlock1) should be(true)
    node.casperEff.contains(signedBlock1Prime) should be(false) // Ignores addition of equivocation pair
  }

  // See [[/docs/casper/images/minimal_equivocation_neglect.png]] but cross out genesis block
  it should "not ignore equivocation blocks that are required for parents of proper nodes" in {
    val nodes   = HashSetCasperTestNode.network(validatorKeys.take(3), genesis)
    val deploys = (0 to 5).map(i => ProtoUtil.basicDeploy(i))

    // Creates a pair that constitutes equivocation blocks
    val Some(signedBlock1) = nodes(0).casperEff.deploy(deploys(0)) *> nodes(0).casperEff.createBlock
    val Some(signedBlock1Prime) = nodes(0).casperEff
      .deploy(deploys(1)) *> nodes(0).casperEff.createBlock

    nodes(1).casperEff.addBlock(signedBlock1)
    nodes(0).transportLayerEff.msgQueues(nodes(0).local).clear //nodes(0) misses this block
    nodes(2).transportLayerEff.msgQueues(nodes(2).local).clear //nodes(2) misses this block

    nodes(0).casperEff.addBlock(signedBlock1Prime)
    nodes(1).transportLayerEff.msgQueues(nodes(1).local).clear //nodes(1) misses this block
    nodes(2).receive()

    nodes(1).casperEff.contains(signedBlock1) should be(true)
    nodes(2).casperEff.contains(signedBlock1) should be(false)
    nodes(1).casperEff.contains(signedBlock1Prime) should be(false)
    nodes(2).casperEff.contains(signedBlock1Prime) should be(true)

    val Some(signedBlock2) = nodes(1).casperEff.deploy(deploys(2)) *> nodes(1).casperEff.createBlock
    val Some(signedBlock3) = nodes(2).casperEff.deploy(deploys(3)) *> nodes(2).casperEff.createBlock

    nodes(2).casperEff.addBlock(signedBlock3)
    nodes(1).casperEff.addBlock(signedBlock2)
    nodes(2).transportLayerEff.msgQueues(nodes(2).local).clear //nodes(2) ignores block2
    nodes(1).receive() // receives block3; asks for block1'
    nodes(2).receive() // receives request for block1'; sends block1'
    nodes(1).receive() // receives block1'; adds both block3 and block1'

    nodes(1).casperEff.contains(signedBlock3) should be(true)
    nodes(1).casperEff.contains(signedBlock1Prime) should be(true)

    val Some(signedBlock4) = nodes(1).casperEff.deploy(deploys(4)) *> nodes(1).casperEff.createBlock

    nodes(1).casperEff.addBlock(signedBlock4)

    // Node 1 should contain both blocks constituting the equivocation
    nodes(1).casperEff.contains(signedBlock1) should be(true)
    nodes(1).casperEff.contains(signedBlock1Prime) should be(true)

    // Rejected due to neglected equivocation
    nodes(1).casperEff.contains(signedBlock4) should be(false)

    nodes(1).logEff.infos.count(_ startsWith "CASPER: Did not add invalid block ") should be(1)
    nodes(1).logEff.warns.count(_ startsWith "CASPER: About to slash the following ") should be(1)

    nodes(1).casperEff.normalizedInitialFault(ProtoUtil.weightMap(genesis)) should be(
      1f / (1f + 3f + 5f + 7f))
  }

  it should "prepare to slash an block that includes a invalid block pointer" in {
    val nodes   = HashSetCasperTestNode.network(validatorKeys.take(3), genesis)
    val deploys = (0 to 5).map(i => ProtoUtil.basicDeploy(i))

    val Some(signedBlock)  = nodes(0).casperEff.deploy(deploys(0)) *> nodes(0).casperEff.createBlock
    val signedInvalidBlock = signedBlock.withSeqNum(-2) // Invalid seq num

    val postState     = RChainState().withBonds(ProtoUtil.bonds(genesis)).withBlockNumber(2)
    val postStateHash = Blake2b256.hash(postState.toByteArray)
    val header = Header()
      .withPostStateHash(ByteString.copyFrom(postStateHash))
      .withParentsHashList(Seq(signedInvalidBlock.blockHash))
    val blockHash = Blake2b256.hash(header.toByteArray)
    val body      = Body().withPostState(postState).withNewCode(deploys)
    val serializedJustifications =
      Seq(Justification(signedInvalidBlock.sender, signedInvalidBlock.blockHash))
    val serializedBlockHash = ByteString.copyFrom(blockHash)
    val blockThatPointsToInvalidBlock = BlockMessage(serializedBlockHash,
                                                     Some(header),
                                                     Some(body),
                                                     serializedJustifications,
                                                     seqNum = 0)
    val signedBlockThatPointsToInvalidBlock =
      ProtoUtil.signBlock(blockThatPointsToInvalidBlock,
                          nodes(1).casperEff.blockDag,
                          validators(1),
                          validatorKeys(1),
                          "ed25519",
                          Ed25519.sign _)

    nodes(1).casperEff.addBlock(signedBlockThatPointsToInvalidBlock)
    nodes(0).transportLayerEff
      .msgQueues(nodes(0).local)
      .clear // nodes(0) rejects normal adding process for blockThatPointsToInvalidBlock
    val signedInvalidBlockPacketMessage = packet(nodes(1).local, signedInvalidBlock.toByteString)
    nodes(0).transportLayerEff.send(nodes(1).local, signedInvalidBlockPacketMessage)
    nodes(1).receive() // receives signedBlockThatPointsToInvalidBlock; attempts to add both blocks

    nodes(1).logEff.warns.count(_ startsWith "CASPER: Ignoring block ") should be(1)
    nodes(1).logEff.warns.count(_ startsWith "CASPER: About to slash the following ") should be(1)
  }

}

object HashSetCasperTest {
  def blockTuplespaceContents(block: BlockMessage)(
      implicit casper: MultiParentCasper[Id]): String = {
    val tsHash = block.body.get.postState.get.tuplespace
    MultiParentCasper[Id].storageContents(tsHash)
  }
}
