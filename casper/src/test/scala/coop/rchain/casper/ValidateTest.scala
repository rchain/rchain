package coop.rchain.casper

import cats.{Id, Monad}
import cats.data.State
import cats.implicits._
import cats.mtl.implicits._

import com.google.protobuf.ByteString

import coop.rchain.casper.BlockDagState._
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil

import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519

import coop.rchain.p2p.EffectsTestInstances.LogStub

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class ValidateTest extends FlatSpec with Matchers with BeforeAndAfterEach with BlockGenerator {

  type StateWithChain[A] = State[BlockDag, A]
  implicit val log = new LogStub[Id]
  val initState    = BlockDag().copy(currentId = -1)
  val ed25519      = "ed25519"

  override def beforeEach(): Unit =
    log.reset()

  def createChain[F[_]: Monad: BlockDagState](length: Int): F[BlockMessage] =
    (0 until length).foldLeft(createBlock[F](Seq.empty)) {
      case (block, _) =>
        for {
          bprev <- block
          bnext <- createBlock[F](Seq(bprev.blockHash))
        } yield bnext
    }

  def signedBlock(i: Int)(implicit chain: BlockDag, sk: Array[Byte]): BlockMessage = {
    val block = chain.idToBlocks(i)
    ProtoUtil.signBlock(block, sk)
  }

  implicit class ChangeBlockNumber(b: BlockMessage) {
    def withBlockNumber(n: Long): BlockMessage = {
      val body  = b.body.getOrElse(Body())
      val state = body.postState.getOrElse(RChainState())

      b.withBody(body.withPostState(state.withBlockNumber(n)))
    }
  }

  "Block signature validation" should "return false on unknown algorithms" in {
    val chain            = createChain[StateWithChain](2).runS(initState).value
    val unknownAlgorithm = "unknownAlgorithm"
    val rsa              = "RSA"
    val block0           = chain.idToBlocks(0).withSigAlgorithm(unknownAlgorithm)
    val block1           = chain.idToBlocks(1).withSigAlgorithm(rsa)

    Validate.blockSignature[Id](block0) should be(false)
    log.warns.last.contains(s"signature algorithm $unknownAlgorithm is unsupported") should be(true)

    Validate.blockSignature[Id](block1) should be(false)
    log.warns.last.contains(s"signature algorithm $rsa is unsupported") should be(true)
  }

  it should "return false on invalid ed25519 signatures" in {

    implicit val chain   = createChain[StateWithChain](6).runS(initState).value
    implicit val (sk, _) = Ed25519.newKeyPair
    val (_, wrongPk)     = Ed25519.newKeyPair
    val empty            = ByteString.EMPTY
    val invalidKey       = ByteString.copyFrom(Base16.decode("abcdef1234567890"))
    val block0           = signedBlock(0).withSender(empty)
    val block1           = signedBlock(1).withSender(invalidKey)
    val block2           = signedBlock(2).withSender(ByteString.copyFrom(wrongPk))
    val block3           = signedBlock(3).withSig(empty)
    val block4           = signedBlock(4).withSig(invalidKey)
    val block5           = signedBlock(5).withSig(block0.sig) //wrong sig

    val blocks = Vector(block0, block1, block2, block3, block4, block5)

    blocks.exists(Validate.blockSignature[Id]) should be(false)
    log.warns.size should be(blocks.length)
    log.warns.forall(_.contains("signature is invalid")) should be(true)
  }

  it should "return true on valid ed25519 signatures" in {
    val n                = 6
    implicit val chain   = createChain[StateWithChain](n).runS(initState).value
    implicit val (sk, _) = Ed25519.newKeyPair

    (0 until n).forall(i => Validate.blockSignature[Id](signedBlock(i))) should be(true)
    log.warns should be(Nil)
  }

  "Block number validation" should "only accept 0 as the number for a block with no parents" in {
    val chain = createChain[StateWithChain](1).runS(initState).value
    val block = chain.idToBlocks(0)

    Validate.blockNumber[Id](block.withBlockNumber(1), chain) should be(false)
    Validate.blockNumber[Id](block, chain) should be(true)
    log.warns.size should be(1)
    log.warns.head.contains("not zero, but block has no parents") should be(true)
  }

  it should "return false for non-sequential numbering" in {
    val chain = createChain[StateWithChain](2).runS(initState).value
    val block = chain.idToBlocks(1)

    Validate.blockNumber[Id](block.withBlockNumber(17), chain) should be(false)
    Validate.blockNumber[Id](block, chain) should be(true)
    log.warns.size should be(1)
    log.warns.head.contains("is not one more than parent number") should be(true)
  }

  it should "return true for sequential numbering" in {
    val n     = 6
    val chain = createChain[StateWithChain](n).runS(initState).value

    (0 until n).forall(i => Validate.blockNumber[Id](chain.idToBlocks(i), chain)) should be(true)
    log.warns should be(Nil)
  }

  "Parent validation" should "return true for proper justifications and false otherwise" in {
    val validators = Vector(
      ByteString.copyFromUtf8("Validator 1"),
      ByteString.copyFromUtf8("Validator 2"),
      ByteString.copyFromUtf8("Validator 3")
    )
    val bonds = validators.zipWithIndex.map {
      case (v, i) => Bond(v, 2 * i + 1)
    }

    def latestMessages(messages: Seq[BlockMessage]): Map[Validator, BlockHash] =
      messages.map(b => b.sender -> b.blockHash).toMap

    def createValidatorBlock[F[_]: Monad: BlockDagState](parents: Seq[BlockMessage],
                                                         justifications: Seq[BlockMessage],
                                                         validator: Int): F[BlockMessage] =
      createBlock[F](
        parents.map(_.blockHash),
        creator = validators(validator),
        bonds = bonds,
        deploys = Seq(ProtoUtil.basicDeploy(0)),
        justifications = latestMessages(justifications)
      )

    def createChainWithValidators[F[_]: Monad: BlockDagState]: F[BlockMessage] =
      for {
        b0 <- createBlock[F](Seq.empty, bonds = bonds)
        b1 <- createValidatorBlock[F](Seq(b0), Seq.empty, 0)
        b2 <- createValidatorBlock[F](Seq(b0), Seq.empty, 1)
        b3 <- createValidatorBlock[F](Seq(b0), Seq.empty, 2)
        b4 <- createValidatorBlock[F](Seq(b1), Seq(b1), 0)
        b5 <- createValidatorBlock[F](Seq(b3, b2, b1), Seq(b1, b2, b3), 1)
        b6 <- createValidatorBlock[F](Seq(b5), Seq(b1, b4, b5), 0)
        b7 <- createValidatorBlock[F](Seq(b4), Seq(b1, b4, b5), 1) //not highest score parent
        b8 <- createValidatorBlock[F](Seq(b1, b2, b3), Seq(b1, b2, b3), 2) //parents wrong order
        b9 <- createValidatorBlock[F](Seq(b6), Seq.empty, 0) //empty justification
      } yield b9

    val chain = createChainWithValidators[StateWithChain].runS(initState).value
    val b0    = chain.idToBlocks(0)

    (0 to 6).forall(i => Validate.parents[Id](chain.idToBlocks(i), b0, chain)) should be(true)
    (7 to 9).exists(i => Validate.parents[Id](chain.idToBlocks(i), b0, chain)) should be(false)
    log.warns.size should be(3)
    log.warns.last.contains("justification is empty, but block has non-genesis parents") should be(
      true)
    log.warns
      .dropRight(1)
      .forall(_.contains("block parents did not match estimate based on justification")) should be(
      true)
  }
}
