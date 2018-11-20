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
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.{ProofOfStake, ProofOfStakeValidator, Rev}
import coop.rchain.casper.helper.{BlockGenerator, BlockStoreFixture, IndexedBlockDag}
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
  val initState    = IndexedBlockDag.empty.copy(currentId = -1)
  val ed25519      = "ed25519"

  override def beforeEach(): Unit = {
    log.reset()
    timeEff.reset()
  }

  def createChain[F[_]: Monad: BlockDagState: Time: BlockStore](
      length: Int,
      bonds: Seq[Bond] = Seq.empty[Bond]
  ): F[BlockMessage] =
    (0 until length).foldLeft(createBlock[F](Seq.empty, bonds = bonds)) {
      case (block, _) =>
        for {
          bprev <- block
          bnext <- createBlock[F](Seq(bprev.blockHash), bonds = bonds)
        } yield bnext
    }

  def createChainWithRoundRobinValidators[F[_]: Monad: BlockDagState: Time: BlockStore](
      length: Int,
      validatorLength: Int
  ): F[BlockMessage] = {
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
            bnext <- createBlock[F](
                      parentsHashList = Seq(block.blockHash),
                      creator = creator,
                      justifications = latestMessages
                    )
            latestMessagesNext = latestMessages.updated(bnext.sender, bnext.blockHash)
          } yield (bnext, latestMessagesNext)
      }
      .map(_._1)
  }

  def signedBlock(i: Int)(implicit chain: IndexedBlockDag, sk: Array[Byte]): BlockMessage = {
    val block = chain.idToBlocks(i)
    val pk    = Ed25519.toPublic(sk)
    ProtoUtil.signBlock(block, chain, pk, sk, "ed25519", "rchain")
  }

  implicit class ChangeBlockNumber(b: BlockMessage) {
    def withBlockNumber(n: Long): BlockMessage = {
      val body     = b.body.getOrElse(Body())
      val state    = body.getState
      val newState = state.withBlockNumber(n)

      val header    = b.header.getOrElse(Header())
      val newHeader = header.withPostStateHash(ProtoUtil.protoHash(newState))

      b.withBody(body.withState(newState)).withHeader(newHeader)
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
        true
      )

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
        Left(InvalidUnslashableBlock)
      )
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
        Left(InvalidUnslashableBlock)
      )
      Validate.timestamp[Id](block, chain) should be(Right(Valid))
      log.warns.size should be(1)
      log.warns.head.contains("block timestamp") should be(true)
  }

  "Block number validation" should "only accept 0 as the number for a block with no parents" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val chain                    = createChain[StateWithChain](1).runS(initState)
      val block                    = chain.idToBlocks(0)

      Validate.blockNumber[Id](block.withBlockNumber(1)) should be(Left(InvalidBlockNumber))
      Validate.blockNumber[Id](block) should be(Right(Valid))
      log.warns.size should be(1)
      log.warns.head.contains("not zero, but block has no parents") should be(true)
  }

  it should "return false for non-sequential numbering" in withStore { implicit blockStore =>
    implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
    val chain                    = createChain[StateWithChain](2).runS(initState)
    val block                    = chain.idToBlocks(1)

    Validate.blockNumber[Id](block.withBlockNumber(17)) should be(Left(InvalidBlockNumber))
    Validate.blockNumber[Id](block) should be(Right(Valid))
    log.warns.size should be(1)
    log.warns.head.contains("is not one more than maximum parent number") should be(true)
  }

  it should "return true for sequential numbering" in withStore { implicit blockStore =>
    implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
    val n                        = 6
    val chain                    = createChain[StateWithChain](n).runS(initState)

    (0 until n).forall(i => Validate.blockNumber[Id](chain.idToBlocks(i)) == Right(Valid)) should be(
      true
    )
    log.warns should be(Nil)
  }

  it should "correctly validate a multiparent block where the parents have different block numbers" in withStore {
    implicit blockStore =>
      def createBlockWithNumber(n: Long, parentHashes: Seq[ByteString] = Nil): BlockMessage = {
        val blockWithNumber = BlockMessage.defaultInstance.withBlockNumber(n)
        val header          = blockWithNumber.getHeader.withParentsHashList(parentHashes)
        val hash            = ProtoUtil.hashUnsignedBlock(header, Nil)
        val block           = blockWithNumber.withHeader(header).withBlockHash(hash)

        blockStore.put(hash, block)
        block
      }
      val b1 = createBlockWithNumber(3)
      val b2 = createBlockWithNumber(7)
      val b3 = createBlockWithNumber(8, Seq(b1.blockHash, b2.blockHash))

      Validate.blockNumber[Id](b3) shouldBe Right(Valid)
      Validate.blockNumber[Id](b3.withBlockNumber(4)) shouldBe Left(InvalidBlockNumber)
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
      true
    )
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
        case (v, i) => Bond(v, 2L * i.toLong + 1L)
      }

      def latestMessages(messages: Seq[BlockMessage]): Map[Validator, BlockHash] =
        messages.map(b => b.sender -> b.blockHash).toMap

      def createValidatorBlock[F[_]: Monad: BlockDagState: Time: BlockStore](
          parents: Seq[BlockMessage],
          justifications: Seq[BlockMessage],
          validator: Int
      ): F[BlockMessage] =
        createBlock[F](
          parents.map(_.blockHash),
          creator = validators(validator),
          bonds = bonds,
          deploys = Seq(ProtoUtil.basicProcessedDeploy[Id](0)),
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
        true
      )
      (7 to 9).exists(i => Validate.parents[Id](chain.idToBlocks(i), b0, chain) == Right(Valid)) should be(
        false
      )
      log.warns.size should be(3)
      log.warns.forall(_.contains("block parents did not match estimate based on justification")) should be(
        true
      )
  }

  // Creates a block with an invalid block number and sequence number
  "Block summary validation" should "short circuit after first invalidity" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val chain                    = createChain[StateWithChain](2).runS(initState)
      val block                    = chain.idToBlocks(1)
      val (sk, pk)                 = Ed25519.newKeyPair

      Validate.blockSummary[Id](
        ProtoUtil.signBlock(
          block.withBlockNumber(17).withSeqNum(1),
          BlockDag.empty,
          pk,
          sk,
          "ed25519",
          "rchain"
        ),
        BlockMessage(),
        chain,
        "rchain"
      ) should be(Left(InvalidBlockNumber))
      log.warns.size should be(1)
  }

  "Justification follow validation" should "return valid for proper justifications and failed otherwise" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val v1                       = ByteString.copyFromUtf8("Validator One")
      val v2                       = ByteString.copyFromUtf8("Validator Two")
      val v1Bond                   = Bond(v1, 2)
      val v2Bond                   = Bond(v2, 3)
      val bonds                    = Seq(v1Bond, v2Bond)

      def createChainWithValidators[F[_]: Monad: BlockDagState: Time: BlockStore]: F[BlockMessage] =
        for {
          genesis <- createBlock[F](Seq(), ByteString.EMPTY, bonds)
          b2 <- createBlock[F](
                 Seq(genesis.blockHash),
                 v2,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
               )
          b3 <- createBlock[F](
                 Seq(genesis.blockHash),
                 v1,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
               )
          b4 <- createBlock[F](
                 Seq(b2.blockHash),
                 v2,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash)
               )
          b5 <- createBlock[F](
                 Seq(b2.blockHash),
                 v1,
                 bonds,
                 HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash)
               )
          _ <- createBlock[F](
                Seq(b4.blockHash),
                v2,
                bonds,
                HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
              )
          b7 <- createBlock[F](
                 Seq(b4.blockHash),
                 v1,
                 Seq(),
                 HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
               )
          b8 <- createBlock[F](
                 Seq(b7.blockHash),
                 v1,
                 bonds,
                 HashMap(v1 -> b7.blockHash, v2 -> b4.blockHash)
               )
        } yield b8

      val chain   = createChainWithValidators[StateWithChain].runS(initState)
      val genesis = chain.idToBlocks(1)

      (1 to 6).forall(
        i =>
          Validate
            .justificationFollows[Id](chain.idToBlocks(i), genesis, chain) == Right(Valid)
      ) should be(true)

      Validate
        .justificationFollows[Id](chain.idToBlocks(7), genesis, chain) == Left(InvalidFollows) should be(
        true
      )

      log.warns.size should be(1)
      log.warns.forall(_.contains("do not match the bonded validators")) should be(
        true
      )
  }

  "Justification regression validation" should "return valid for proper justifications and justification regression detected otherwise" in withStore {
    implicit blockStore =>
      implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)
      val validators = Vector(
        ByteString.copyFromUtf8("Validator 1"),
        ByteString.copyFromUtf8("Validator 2")
      )
      val bonds = validators.zipWithIndex.map {
        case (v, i) => Bond(v, 2L * i.toLong + 1L)
      }

      def latestMessages(messages: Seq[BlockMessage]): Map[Validator, BlockHash] =
        messages.map(b => b.sender -> b.blockHash).toMap

      def createValidatorBlock[F[_]: Monad: BlockDagState: Time: BlockStore](
          parents: Seq[BlockMessage],
          justifications: Seq[BlockMessage],
          validator: Int
      ): F[BlockMessage] =
        createBlock[F](
          parents.map(_.blockHash),
          creator = validators(validator),
          bonds = bonds,
          deploys = Seq(ProtoUtil.basicProcessedDeploy[Id](0)),
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

      (0 to 4).forall(
        i =>
          Validate
            .justificationRegressions[Id](chain.idToBlocks(i), b0, chain) == Right(Valid)
      ) should be(true)

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
        Left(JustificationRegression)
      )
      log.warns.size should be(1)
  }

  "Bonds cache validation" should "succeed on a valid block and fail on modified bonds" in withStore {
    implicit blockStore =>
      val (_, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
      val bonds           = HashSetCasperTest.createBonds(validators)
      val genesis         = HashSetCasperTest.createGenesis(bonds)

      val storageDirectory  = Files.createTempDirectory(s"hash-set-casper-test-genesis")
      val storageSize: Long = 1024L * 1024
      val activeRuntime     = Runtime.create(storageDirectory, storageSize)
      val runtimeManager    = RuntimeManager.fromRuntime(activeRuntime)
      val _ = InterpreterUtil
        .validateBlockCheckpoint[Id](genesis, BlockDag.empty, runtimeManager)

      Validate.bondsCache[Id](genesis, runtimeManager) should be(Right(Valid))

      val modifiedBonds     = Seq.empty[Bond]
      val modifiedPostState = genesis.getBody.getState.withBonds(modifiedBonds)
      val modifiedBody      = genesis.getBody.withState(modifiedPostState)
      val modifiedGenesis   = genesis.withBody(modifiedBody)
      Validate.bondsCache[Id](modifiedGenesis, runtimeManager) should be(Left(InvalidBondsCache))

      activeRuntime.close().unsafeRunSync
  }

  "Field format validation" should "succeed on a valid block and fail on empty fields" in {
    val (sk, pk) = Ed25519.newKeyPair
    val block    = HashSetCasperTest.createGenesis(Map(pk -> 1))
    val genesis =
      ProtoUtil.signBlock(block, BlockDag.empty, pk, sk, "ed25519", "rchain")

    Validate.formatOfFields[Id](genesis) should be(true)
    Validate.formatOfFields[Id](genesis.withBlockHash(ByteString.EMPTY)) should be(false)
    Validate.formatOfFields[Id](genesis.clearHeader) should be(false)
    Validate.formatOfFields[Id](genesis.clearBody) should be(false)
    Validate.formatOfFields[Id](genesis.withSig(ByteString.EMPTY)) should be(false)
    Validate.formatOfFields[Id](genesis.withSigAlgorithm("")) should be(false)
    Validate.formatOfFields[Id](genesis.withShardId("")) should be(false)
    Validate.formatOfFields[Id](genesis.withBody(genesis.getBody.clearState)) should be(false)
    Validate.formatOfFields[Id](
      genesis.withHeader(genesis.header.get.withPostStateHash(ByteString.EMPTY))
    ) should be(false)
    Validate.formatOfFields[Id](
      genesis.withHeader(genesis.header.get.withDeploysHash(ByteString.EMPTY))
    ) should be(false)
    Validate.formatOfFields[Id](
      genesis.withBody(
        genesis.body.get
          .withDeploys(genesis.body.get.deploys.map(_.withLog(List(Event(EventInstance.Empty)))))
      )
    ) should be(false)
  }

  "Block hash format validation" should "fail on invalid hash" in {
    val (sk, pk) = Ed25519.newKeyPair
    val block    = HashSetCasperTest.createGenesis(Map(pk -> 1))
    val genesis  = ProtoUtil.signBlock(block, BlockDag.empty, pk, sk, "ed25519", "rchain")
    Validate.blockHash[Id](genesis) should be(Right(Valid))
    Validate.blockHash[Id](
      genesis.withBlockHash(ByteString.copyFromUtf8("123"))
    ) should be(Left(InvalidBlockHash))
  }

  "Block deploy count validation" should "fail on invalid number of deploys" in {
    val (sk, pk) = Ed25519.newKeyPair
    val block    = HashSetCasperTest.createGenesis(Map(pk -> 1))
    val genesis  = ProtoUtil.signBlock(block, BlockDag.empty, pk, sk, "ed25519", "rchain")
    Validate.deployCount[Id](genesis) should be(Right(Valid))
    Validate.deployCount[Id](
      genesis.withHeader(genesis.header.get.withDeployCount(100))
    ) should be(Left(InvalidDeployCount))
  }

  "Block version validation" should "work" in {
    val (sk, pk) = Ed25519.newKeyPair
    val block    = HashSetCasperTest.createGenesis(Map(pk -> 1))
    val genesis  = ProtoUtil.signBlock(block, BlockDag.empty, pk, sk, "ed25519", "rchain")
    Validate.version[Id](genesis, -1) should be(false)
    Validate.version[Id](genesis, 1) should be(true)
  }
}
