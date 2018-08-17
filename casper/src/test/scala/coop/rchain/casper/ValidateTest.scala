package coop.rchain.casper

import java.nio.file.Files

import cats.effect.Bracket
import cats.{Id, Monad}
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.{ProofOfStake, ProofOfStakeValidator, Rev}
import coop.rchain.casper.helper.{BlockGenerator, BlockStoreFixture}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.protocol.Event.EventInstance
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.ProtoUtil.termDeploy
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.models.Par
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.rholang.collection.LinkedList
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.math.NonNegativeNumber
import coop.rchain.rholang.mint.MakeMint
import coop.rchain.rholang.wallet.BasicWallet
import coop.rchain.shared.Time
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

import scala.collection.immutable.HashMap

class ValidateTest
    extends FlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BlockGenerator
    with BlockStoreFixture {
  implicit val log = new LogStub[Id]
  val initState    = BlockDag().copy(currentId = -1)
  val ed25519      = "ed25519"

  override def beforeEach(): Unit = {
    log.reset()
    timeEff.reset()
  }

  def createChain[F[_]: Monad: BlockDagState: Time: BlockStore](
      length: Int,
      bonds: Seq[Bond] = Seq.empty[Bond]): F[BlockMessage] =
    (0 until length).foldLeft(createBlock[F](Seq.empty, bonds = bonds)) {
      case (block, _) =>
        for {
          bprev <- block
          bnext <- createBlock[F](Seq(bprev.blockHash), bonds = bonds)
        } yield bnext
    }

  def createChainWithRoundRobinValidators[F[_]: Monad: BlockDagState: Time: BlockStore](
      length: Int,
      validatorLength: Int): F[BlockMessage] = {
    val validatorRoundRobinCycle = Stream.continually(0 until validatorLength).flatten
    (0 until length).toList
      .zip(validatorRoundRobinCycle)
      .foldLeft(
        for {
          genesis             <- createBlock[F](Seq.empty)
          emptyLatestMessages <- HashMap.empty[Validator, BlockHash].pure[F]
        } yield (genesis, emptyLatestMessages)
      ) {
        case (acc, (_, validatorNum)) =>
          val creator = ByteString.copyFrom(validatorNum.toString.getBytes)
          for {
            unwrappedAcc            <- acc
            (block, latestMessages) = unwrappedAcc
            bnext <- createBlock[F](parentsHashList = Seq(block.blockHash),
                                    creator = creator,
                                    justifications = latestMessages)
            latestMessagesNext = latestMessages.updated(bnext.sender, bnext.blockHash)
          } yield (bnext, latestMessagesNext)
      }
      .map(_._1)
  }

  def signedBlock(i: Int)(implicit chain: BlockDag, sk: Array[Byte]): BlockMessage = {
    val block = chain.idToBlocks(i)
    val pk    = Ed25519.toPublic(sk)
    ProtoUtil.signBlock(block, chain, pk, sk, "ed25519", Ed25519.sign _, "rchain")
  }

  implicit class ChangeBlockNumber(b: BlockMessage) {
    def withBlockNumber(n: Long): BlockMessage = {
      val body  = b.body.getOrElse(Body())
      val state = body.postState.getOrElse(RChainState())

      b.withBody(body.withPostState(state.withBlockNumber(n)))
    }
  }

  "Block signature validation" should "return false on unknown algorithms" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val chain                    = createChain[StateWithChain](2).runS(initState)
      val unknownAlgorithm         = "unknownAlgorithm"
      val rsa                      = "RSA"
      val block0                   = chain.idToBlocks(0).withSigAlgorithm(unknownAlgorithm)
      val block1                   = chain.idToBlocks(1).withSigAlgorithm(rsa)

      Validate.blockSignature[Id](block0) should be(false)
      log.warns.last.contains(s"signature algorithm $unknownAlgorithm is unsupported") should be(
        true)

      Validate.blockSignature[Id](block1) should be(false)
      log.warns.last.contains(s"signature algorithm $rsa is unsupported") should be(true)
  }

  it should "return false on invalid ed25519 signatures" in withStore { implicit blockStore =>
    implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
    implicit val chain           = createChain[StateWithChain](6).runS(initState)
    implicit val (sk, _)         = Ed25519.newKeyPair
    val (_, wrongPk)             = Ed25519.newKeyPair
    val empty                    = ByteString.EMPTY
    val invalidKey               = ByteString.copyFrom(Base16.decode("abcdef1234567890"))
    val block0                   = signedBlock(0).withSender(empty)
    val block1                   = signedBlock(1).withSender(invalidKey)
    val block2                   = signedBlock(2).withSender(ByteString.copyFrom(wrongPk))
    val block3                   = signedBlock(3).withSig(empty)
    val block4                   = signedBlock(4).withSig(invalidKey)
    val block5                   = signedBlock(5).withSig(block0.sig) //wrong sig

    val blocks = Vector(block0, block1, block2, block3, block4, block5)

    blocks.exists(Validate.blockSignature[Id]) should be(false)
    log.warns.size should be(blocks.length)
    log.warns.forall(_.contains("signature is invalid")) should be(true)
  }

  it should "return true on valid ed25519 signatures" in withStore { implicit blockStore =>
    implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
    val n                        = 6
    implicit val chain           = createChain[StateWithChain](n).runS(initState)
    implicit val (sk, _)         = Ed25519.newKeyPair

    (0 until n).forall(i => Validate.blockSignature[Id](signedBlock(i))) should be(true)
    log.warns should be(Nil)
  }

  "Timestamp validation" should "not accept blocks with future time" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val chain                    = createChain[StateWithChain](1).runS(initState)
      val block                    = chain.idToBlocks(0)

      val modifiedTimestampHeader = block.header.get.withTimestamp(99999999)
      Validate.timestamp[Id](block.withHeader(modifiedTimestampHeader), chain) should be(
        Left(InvalidUnslashableBlock))
      Validate.timestamp[Id](block, chain) should be(Right(Valid))

      log.warns.size should be(1)
      log.warns.head.contains("block timestamp") should be(true)
  }

  it should "not accept blocks that were published before parent time" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val chain                    = createChain[StateWithChain](2).runS(initState)
      val block                    = chain.idToBlocks(1)

      val modifiedTimestampHeader = block.header.get.withTimestamp(-1)
      Validate.timestamp[Id](block.withHeader(modifiedTimestampHeader), chain) should be(
        Left(InvalidUnslashableBlock))
      Validate.timestamp[Id](block, chain) should be(Right(Valid))
      log.warns.size should be(1)
      log.warns.head.contains("block timestamp") should be(true)
  }

  "Block number validation" should "only accept 0 as the number for a block with no parents" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val chain                    = createChain[StateWithChain](1).runS(initState)
      val block                    = chain.idToBlocks(0)

      Validate.blockNumber[Id](block.withBlockNumber(1), chain) should be(Left(InvalidBlockNumber))
      Validate.blockNumber[Id](block, chain) should be(Right(Valid))
      log.warns.size should be(1)
      log.warns.head.contains("not zero, but block has no parents") should be(true)
  }

  it should "return false for non-sequential numbering" in withStore { implicit blockStore =>
    implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
    val chain                    = createChain[StateWithChain](2).runS(initState)
    val block                    = chain.idToBlocks(1)

    Validate.blockNumber[Id](block.withBlockNumber(17), chain) should be(Left(InvalidBlockNumber))
    Validate.blockNumber[Id](block, chain) should be(Right(Valid))
    log.warns.size should be(1)
    log.warns.head.contains("is not one more than parent number") should be(true)
  }

  it should "return true for sequential numbering" in withStore { implicit blockStore =>
    implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
    val n                        = 6
    val chain                    = createChain[StateWithChain](n).runS(initState)

    (0 until n).forall(i => Validate.blockNumber[Id](chain.idToBlocks(i), chain) == Right(Valid)) should be(
      true)
    log.warns should be(Nil)
  }

  "Sequence number validation" should "only accept 0 as the number for a block with no parents" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val chain                    = createChain[StateWithChain](1).runS(initState)
      val block                    = chain.idToBlocks(0)

      Validate.sequenceNumber[Id](block.withSeqNum(1), chain) should be(Left(InvalidSequenceNumber))
      Validate.sequenceNumber[Id](block, chain) should be(Right(Valid))
      log.warns.size should be(1)
  }

  it should "return false for non-sequential numbering" in withStore { implicit blockStore =>
    implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
    val chain                    = createChain[StateWithChain](2).runS(initState)
    val block                    = chain.idToBlocks(1)

    Validate.sequenceNumber[Id](block.withSeqNum(1), chain) should be(Left(InvalidSequenceNumber))
    log.warns.size should be(1)
  }

  it should "return true for sequential numbering" in withStore { implicit blockStore =>
    implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
    val n                        = 20
    val validatorCount           = 3
    val chain =
      createChainWithRoundRobinValidators[StateWithChain](n, validatorCount).runS(initState)

    (0 until n).forall(i => Validate.sequenceNumber[Id](chain.idToBlocks(i), chain) == Right(Valid)) should be(
      true)
    log.warns should be(Nil)
  }

  "Sender validation" should "return true for genesis and blocks from bonded validators and false otherwise" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val validator                = ByteString.copyFromUtf8("Validator")
      val impostor                 = ByteString.copyFromUtf8("Impostor")
      val chain                    = createChain[StateWithChain](3, List(Bond(validator, 1))).runS(initState)
      val genesis                  = chain.idToBlocks(0)
      val validBlock               = chain.idToBlocks(1).withSender(validator)
      val invalidBlock             = chain.idToBlocks(2).withSender(impostor)

      Validate.blockSender[Id](genesis, genesis, chain) should be(true)
      Validate.blockSender[Id](validBlock, genesis, chain) should be(true)
      Validate.blockSender[Id](invalidBlock, genesis, chain) should be(false)
  }

  "Parent validation" should "return true for proper justifications and false otherwise" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
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

      def createValidatorBlock[F[_]: Monad: BlockDagState: Time: BlockStore](
          parents: Seq[BlockMessage],
          justifications: Seq[BlockMessage],
          validator: Int): F[BlockMessage] =
        createBlock[F](
          parents.map(_.blockHash),
          creator = validators(validator),
          bonds = bonds,
          deploys = Seq(ProtoUtil.basicDeployCost(0)),
          justifications = latestMessages(justifications)
        )

      def createChainWithValidators[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
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

      val chain = createChainWithValidators[StateWithChain].runS(initState)
      val b0    = chain.idToBlocks(0)

      (0 to 6).forall(i => Validate.parents[Id](chain.idToBlocks(i), b0, chain) == Right(Valid)) should be(
        true)
      (7 to 9).exists(i => Validate.parents[Id](chain.idToBlocks(i), b0, chain) == Right(Valid)) should be(
        false)
      log.warns.size should be(3)
      log.warns.last
        .contains("justification is empty, but block has non-genesis parents") should be(true)
      log.warns
        .dropRight(1)
        .forall(_.contains("block parents did not match estimate based on justification")) should be(
        true)
  }

  // Creates a block with an invalid block number and sequence number
  "Block summary validation" should "short circuit after first invalidity" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val chain                    = createChain[StateWithChain](2).runS(initState)
      val block                    = chain.idToBlocks(1)

      Validate.blockSummary[Id](
        block.withBlockNumber(17).withSeqNum(1),
        BlockMessage(),
        chain,
        "rchain"
      ) should be(Left(InvalidBlockNumber))
      log.warns.size should be(1)
  }

  "Justification regression validation" should "return valid for proper justifications and justification regression detected otherwise" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val validators = Vector(
        ByteString.copyFromUtf8("Validator 1"),
        ByteString.copyFromUtf8("Validator 2")
      )
      val bonds = validators.zipWithIndex.map {
        case (v, i) => Bond(v, 2 * i + 1)
      }

      def latestMessages(messages: Seq[BlockMessage]): Map[Validator, BlockHash] =
        messages.map(b => b.sender -> b.blockHash).toMap

      def createValidatorBlock[F[_]: Monad: BlockDagState: Time: BlockStore](
          parents: Seq[BlockMessage],
          justifications: Seq[BlockMessage],
          validator: Int): F[BlockMessage] =
        createBlock[F](
          parents.map(_.blockHash),
          creator = validators(validator),
          bonds = bonds,
          deploys = Seq(ProtoUtil.basicDeployCost(0)),
          justifications = latestMessages(justifications)
        )

      def createChainWithValidators[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
        for {
          b0 <- createBlock[F](Seq.empty, bonds = bonds)
          b1 <- createValidatorBlock[F](Seq(b0), Seq(b0, b0), 0)
          b2 <- createValidatorBlock[F](Seq(b1), Seq(b1, b0), 0)
          b3 <- createValidatorBlock[F](Seq(b0), Seq(b2, b0), 1)
          b4 <- createValidatorBlock[F](Seq(b3), Seq(b2, b3), 1)
        } yield b4

      val chain = createChainWithValidators[StateWithChain].runS(initState)
      val b0    = chain.idToBlocks(0)

      (0 to 4).forall(i =>
        Validate
          .justificationRegressions[Id](chain.idToBlocks(i), b0, chain) == Right(Valid)) should be(
        true)

      // The justification block for validator 0 should point to b2 or above.
      val b1 = chain.idToBlocks(1)
      val b4 = chain.idToBlocks(4)
      val justificationsWithRegression =
        Seq(Justification(validators(0), b1.blockHash), Justification(validators(1), b4.blockHash))
      val blockWithJustificationRegression =
        BlockMessage()
          .withSender(validators(1))
          .withJustifications(justificationsWithRegression)
      Validate.justificationRegressions[Id](blockWithJustificationRegression, b0, chain) should be(
        Left(JustificationRegression))
      log.warns.size should be(1)
  }

  "Bonds cache validation" should "succeed on a valid block and fail on modified bonds" in {
    val (_, validators)   = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
    val bonds             = validators.zipWithIndex.map { case (v, i) => v -> (2 * i + 1) }.toMap
    val initial           = Genesis.withoutContracts(bonds, 0L, 0L, "rchain")
    val storageDirectory  = Files.createTempDirectory(s"hash-set-casper-test-genesis")
    val storageSize: Long = 1024L * 1024
    val activeRuntime     = Runtime.create(storageDirectory, storageSize)
    val runtimeManager    = RuntimeManager.fromRuntime(activeRuntime)
    val emptyStateHash    = runtimeManager.emptyStateHash

    val proofOfStakeValidators = bonds.map(bond => ProofOfStakeValidator(bond._1, bond._2)).toSeq
    val proofOfStakeStubPar    = new ProofOfStake(proofOfStakeValidators).term
    val genesis = Genesis.withContracts(List(ProtoUtil.termDeploy(proofOfStakeStubPar)),
                                        initial,
                                        emptyStateHash,
                                        runtimeManager)

    Validate.bondsCache[Id](genesis, runtimeManager) should be(Right(Valid))

    val modifiedBonds     = Seq.empty[Bond]
    val modifiedPostState = genesis.body.get.postState.get.withBonds(modifiedBonds)
    val modifiedBody      = genesis.body.get.withPostState(modifiedPostState)
    val modifiedGenesis   = genesis.withBody(modifiedBody)
    Validate.bondsCache[Id](modifiedGenesis, runtimeManager) should be(Left(InvalidBondsCache))

    activeRuntime.close()
  }

  "Field format validation" should "succeed on a valid block and fail on empty fields" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      implicit val chain           = createChain[StateWithChain](1).runS(initState)
      implicit val (sk, _)         = Ed25519.newKeyPair
      val genesis                  = signedBlock(0)
      Validate.formatOfFields[Id](genesis) should be(true)
      Validate.formatOfFields[Id](genesis.withBlockHash(ByteString.EMPTY)) should be(false)
      Validate.formatOfFields[Id](genesis.clearHeader) should be(false)
      Validate.formatOfFields[Id](genesis.clearBody) should be(false)
      Validate.formatOfFields[Id](genesis.withSig(ByteString.EMPTY)) should be(false)
      Validate.formatOfFields[Id](genesis.withSigAlgorithm("")) should be(false)
      Validate.formatOfFields[Id](genesis.withShardId("")) should be(false)
      Validate.formatOfFields[Id](genesis.withBody(genesis.body.get.clearPostState)) should be(
        false)
      Validate.formatOfFields[Id](
        genesis.withHeader(genesis.header.get.withPostStateHash(ByteString.EMPTY))
      ) should be(false)
      Validate.formatOfFields[Id](
        genesis.withHeader(genesis.header.get.withNewCodeHash(ByteString.EMPTY))
      ) should be(false)
      Validate.formatOfFields[Id](
        genesis.withHeader(genesis.header.get.withCommReductionsHash(ByteString.EMPTY))
      ) should be(false)
      Validate.formatOfFields[Id](
        genesis.withBody(genesis.body.get.withCommReductions(List(Event(EventInstance.Empty))))
      ) should be(false)
  }
}
