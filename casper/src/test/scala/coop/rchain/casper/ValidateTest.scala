package coop.rchain.casper

import java.nio.file.Files

import cats.effect.Bracket
import cats.{Id, Monad}
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagStorage, BlockStore, IndexedBlockDagStorage}
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.{ProofOfStake, ProofOfStakeValidator, Rev}
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator, BlockStoreFixture}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.protocol.Event.EventInstance
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.casper.util.ProtoUtil.{conflicts, termDeploy}
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.casper.util.rholang.{InterpreterUtil, ProcessedDeployUtil, RuntimeManager}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.models.Par
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.math.NonNegativeNumber
import coop.rchain.rholang.mint.MakeMint
import coop.rchain.rholang.wallet.BasicWallet
import coop.rchain.shared.Time
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import scala.concurrent.duration._

import scala.collection.immutable.HashMap

class ValidateTest
    extends FlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BlockGenerator
    with BlockStoreFixture
    with BlockDagStorageFixture {
  implicit val log = new LogStub[Id]
  val ed25519      = "ed25519"

  override def beforeEach(): Unit = {
    log.reset()
    timeEff.reset()
  }

  def createChain[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
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

  def createChainWithRoundRobinValidators[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
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

  def signedBlock(
      i: Int
  )(implicit sk: Array[Byte], blockDagStorage: IndexedBlockDagStorage[Id]): BlockMessage = {
    val block = blockDagStorage.lookupByIdUnsafe(i)
    val pk    = Ed25519.toPublic(sk)
    ProtoUtil.signBlock[Id](block, blockDagStorage.getRepresentation, pk, sk, "ed25519", "rchain")
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
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        createChain[Id](2)

        val unknownAlgorithm = "unknownAlgorithm"
        val rsa              = "RSA"
        val block0           = blockDagStorage.lookupByIdUnsafe(0).withSigAlgorithm(unknownAlgorithm)
        val block1           = blockDagStorage.lookupByIdUnsafe(1).withSigAlgorithm(rsa)

        Validate.blockSignature[Id](block0) should be(false)
        log.warns.last
          .contains(s"signature algorithm $unknownAlgorithm is unsupported") should be(
          true
        )

        Validate.blockSignature[Id](block1) should be(false)
        log.warns.last.contains(s"signature algorithm $rsa is unsupported") should be(true)
      }
  }

  it should "return false on invalid ed25519 signatures" in withStore { implicit blockStore =>
    withIndexedBlockDagStorage { implicit blockDagStorage =>
      createChain[Id](6)
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
  }

  it should "return true on valid ed25519 signatures" in withStore { implicit blockStore =>
    withIndexedBlockDagStorage { implicit blockDagStorage =>
      val n = 6
      createChain[Id](n)
      implicit val (sk, _) = Ed25519.newKeyPair

      (0 until n).forall(i => Validate.blockSignature[Id](signedBlock(i))) should be(true)
      log.warns should be(Nil)
    }
  }

  "Timestamp validation" should "not accept blocks with future time" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        createChain[Id](1)
        val block = blockDagStorage.lookupByIdUnsafe(0)

        val modifiedTimestampHeader = block.header.get.withTimestamp(99999999)
        Validate.timestamp[Id](
          block.withHeader(modifiedTimestampHeader),
          blockDagStorage.getRepresentation
        ) should be(
          Left(InvalidUnslashableBlock)
        )
        Validate.timestamp[Id](block, blockDagStorage.getRepresentation) should be(Right(Valid))

        log.warns.size should be(1)
        log.warns.head.contains("block timestamp") should be(true)
      }
  }

  it should "not accept blocks that were published before parent time" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        createChain[Id](2)
        val block = blockDagStorage.lookupByIdUnsafe(1)

        val modifiedTimestampHeader = block.header.get.withTimestamp(-1)
        Validate.timestamp[Id](
          block.withHeader(modifiedTimestampHeader),
          blockDagStorage.getRepresentation
        ) should be(
          Left(InvalidUnslashableBlock)
        )
        Validate.timestamp[Id](block, blockDagStorage.getRepresentation) should be(Right(Valid))
        log.warns.size should be(1)
        log.warns.head.contains("block timestamp") should be(true)
      }
  }

  "Block number validation" should "only accept 0 as the number for a block with no parents" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        createChain[Id](1)
        val block = blockDagStorage.lookupByIdUnsafe(0)

        Validate.blockNumber[Id](block.withBlockNumber(1)) should be(Left(InvalidBlockNumber))
        Validate.blockNumber[Id](block) should be(Right(Valid))
        log.warns.size should be(1)
        log.warns.head.contains("not zero, but block has no parents") should be(true)
      }
  }

  it should "return false for non-sequential numbering" in withStore { implicit blockStore =>
    withIndexedBlockDagStorage { implicit blockDagStorage =>
      createChain[Id](2)
      val block = blockDagStorage.lookupByIdUnsafe(1)

      Validate.blockNumber[Id](block.withBlockNumber(17)) should be(Left(InvalidBlockNumber))
      Validate.blockNumber[Id](block) should be(Right(Valid))
      log.warns.size should be(1)
      log.warns.head.contains("is not one more than maximum parent number") should be(true)
    }
  }

  it should "return true for sequential numbering" in withStore { implicit blockStore =>
    withIndexedBlockDagStorage { implicit blockDagStorage =>
      val n = 6
      createChain[Id](n)
      val a0 = Validate.blockNumber[Id](blockDagStorage.lookupByIdUnsafe(0))
      val a1 = Validate.blockNumber[Id](blockDagStorage.lookupByIdUnsafe(1))
      val a2 = Validate.blockNumber[Id](blockDagStorage.lookupByIdUnsafe(2))
      val a3 = Validate.blockNumber[Id](blockDagStorage.lookupByIdUnsafe(3))
      val a4 = Validate.blockNumber[Id](blockDagStorage.lookupByIdUnsafe(4))
      val a5 = Validate.blockNumber[Id](blockDagStorage.lookupByIdUnsafe(5))

      (0 until n).forall(
        i => Validate.blockNumber[Id](blockDagStorage.lookupByIdUnsafe(i)) == Right(Valid)
      ) should be(
        true
      )
      log.warns should be(Nil)
    }
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
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        createChain[Id](1)
        val block = blockDagStorage.lookupByIdUnsafe(0)

        Validate
          .sequenceNumber[Id](block.withSeqNum(1), blockDagStorage.getRepresentation) should be(
          Left(InvalidSequenceNumber)
        )
        Validate.sequenceNumber[Id](block, blockDagStorage.getRepresentation) should be(
          Right(Valid)
        )
        log.warns.size should be(1)
      }
  }

  it should "return false for non-sequential numbering" in withStore { implicit blockStore =>
    withIndexedBlockDagStorage { implicit blockDagStorage =>
      createChain[Id](2)
      val block = blockDagStorage.lookupByIdUnsafe(1)

      Validate
        .sequenceNumber[Id](block.withSeqNum(1), blockDagStorage.getRepresentation) should be(
        Left(InvalidSequenceNumber)
      )
      log.warns.size should be(1)
    }
  }

  it should "return true for sequential numbering" in withStore { implicit blockStore =>
    withIndexedBlockDagStorage { implicit blockDagStorage =>
      val n              = 20
      val validatorCount = 3
      val chain =
        createChainWithRoundRobinValidators[Id](n, validatorCount)

      (0 until n).forall(
        i =>
          Validate.sequenceNumber[Id](
            blockDagStorage.lookupByIdUnsafe(i),
            blockDagStorage.getRepresentation
          ) == Right(Valid)
      ) should be(
        true
      )
      log.warns should be(Nil)
    }
  }

  "Sender validation" should "return true for genesis and blocks from bonded validators and false otherwise" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        val validator    = ByteString.copyFromUtf8("Validator")
        val impostor     = ByteString.copyFromUtf8("Impostor")
        val chain        = createChain[Id](3, List(Bond(validator, 1)))
        val genesis      = blockDagStorage.lookupByIdUnsafe(0)
        val validBlock   = blockDagStorage.lookupByIdUnsafe(1).withSender(validator)
        val invalidBlock = blockDagStorage.lookupByIdUnsafe(2).withSender(impostor)

        Validate.blockSender[Id](genesis, genesis, blockDagStorage.getRepresentation) should be(
          true
        )
        Validate
          .blockSender[Id](validBlock, genesis, blockDagStorage.getRepresentation) should be(
          true
        )
        Validate
          .blockSender[Id](invalidBlock, genesis, blockDagStorage.getRepresentation) should be(
          false
        )
      }
  }

  "Parent validation" should "return true for proper justifications and false otherwise" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
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

        def createValidatorBlock[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
            parents: Seq[BlockMessage],
            justifications: Seq[BlockMessage],
            validator: Int
        ): F[BlockMessage] =
          createBlock[F](
            parents.map(_.blockHash),
            creator = validators(validator),
            bonds = bonds,
            deploys = Seq(ProtoUtil.basicProcessedDeploy[Id](timeEff.currentMillis.toInt)),
            justifications = latestMessages(justifications)
          )

        val b0 = createBlock[Id](Seq.empty, bonds = bonds)
        val b1 = createValidatorBlock[Id](Seq(b0), Seq.empty, 0)
        val b2 = createValidatorBlock[Id](Seq(b0), Seq.empty, 1)
        val b3 = createValidatorBlock[Id](Seq(b0), Seq.empty, 2)
        val b4 = createValidatorBlock[Id](Seq(b1), Seq(b1), 0)
        val b5 = createValidatorBlock[Id](Seq(b3, b2, b1), Seq(b1, b2, b3), 1)
        val b6 = createValidatorBlock[Id](Seq(b5, b4), Seq(b1, b4, b5), 0)
        val b7 = createValidatorBlock[Id](Seq(b4), Seq(b1, b4, b5), 1) //not highest score parent
        val b8 = createValidatorBlock[Id](Seq(b1, b2, b3), Seq(b1, b2, b3), 2) //parents wrong order
        val b9 = createValidatorBlock[Id](Seq(b6), Seq.empty, 0) //empty justification

      val chain   = createChainWithValidators[StateWithChain].runS(initState)
      val genesis = chain.idToBlocks(0)

      mkRuntimeManager("casper-util-test")
        .use { runtimeManager =>
          Task.delay {
            val (b0: BlockMessage, chainWithUpdatedB0: IndexedBlockDag) =
              updateChainWithBlockStateUpdate(0, genesis, runtimeManager, chain)
            val (b1: BlockMessage, chainWithUpdatedB1: IndexedBlockDag) =
              updateChainWithBlockStateUpdate(1, genesis, runtimeManager, chainWithUpdatedB0)
            val (b2: BlockMessage, chainWithUpdatedB2: IndexedBlockDag) =
              updateChainWithBlockStateUpdate(2, genesis, runtimeManager, chainWithUpdatedB1)
            val (b3: BlockMessage, chainWithUpdatedB3: IndexedBlockDag) =
              updateChainWithBlockStateUpdate(3, genesis, runtimeManager, chainWithUpdatedB2)
            val (b4: BlockMessage, chainWithUpdatedB4: IndexedBlockDag) =
              updateChainWithBlockStateUpdate(4, genesis, runtimeManager, chainWithUpdatedB3)
            val (b5: BlockMessage, chainWithUpdatedB5: IndexedBlockDag) =
              updateChainWithBlockStateUpdate(5, genesis, runtimeManager, chainWithUpdatedB4)
            val (b6: BlockMessage, chainWithUpdatedB6: IndexedBlockDag) =
              updateChainWithBlockStateUpdate(6, genesis, runtimeManager, chainWithUpdatedB5)
            val (b7: BlockMessage, chainWithUpdatedB7: IndexedBlockDag) =
              updateChainWithBlockStateUpdate(7, genesis, runtimeManager, chainWithUpdatedB6)
            val (b8: BlockMessage, chainWithUpdatedB8: IndexedBlockDag) =
              updateChainWithBlockStateUpdate(8, genesis, runtimeManager, chainWithUpdatedB7)
            val (b9: BlockMessage, chainWithUpdatedB9: IndexedBlockDag) =
              updateChainWithBlockStateUpdate(9, genesis, runtimeManager, chainWithUpdatedB8)

            // Valid
            Validate.parents[Id](chainWithUpdatedB0.idToBlocks(0), b0, chainWithUpdatedB0)
            Validate.parents[Id](chainWithUpdatedB1.idToBlocks(1), b0, chainWithUpdatedB1)
            Validate.parents[Id](chainWithUpdatedB2.idToBlocks(2), b0, chainWithUpdatedB2)
            Validate.parents[Id](chainWithUpdatedB3.idToBlocks(3), b0, chainWithUpdatedB3)
            Validate.parents[Id](chainWithUpdatedB4.idToBlocks(4), b0, chainWithUpdatedB4)
            Validate.parents[Id](chainWithUpdatedB5.idToBlocks(5), b0, chainWithUpdatedB5)
            Validate.parents[Id](chainWithUpdatedB6.idToBlocks(6), b0, chainWithUpdatedB6)

            // Not valid
            Validate.parents[Id](chainWithUpdatedB7.idToBlocks(7), b0, chainWithUpdatedB7)
            Validate.parents[Id](chainWithUpdatedB8.idToBlocks(8), b0, chainWithUpdatedB8)
            Validate.parents[Id](chainWithUpdatedB9.idToBlocks(9), b0, chainWithUpdatedB9)

            log.warns.size should be(3)
            log.warns.forall(
              _.contains("block parents did not match estimate based on justification")
            ) should be(
              true
            )
          }
        }
        .runSyncUnsafe(10.seconds)
  }

  // Creates a block with an invalid block number and sequence number
  "Block summary validation" should "short circuit after first invalidity" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        createChain[Id](2)
        val block    = blockDagStorage.lookupByIdUnsafe(1)
        val (sk, pk) = Ed25519.newKeyPair

        Validate.blockSummary[Id](
          ProtoUtil.signBlock[Id](
            block.withBlockNumber(17).withSeqNum(1),
            blockDagStorage.getRepresentation,
            pk,
            sk,
            "ed25519",
            "rchain"
          ),
          BlockMessage(),
          blockDagStorage.getRepresentation,
          "rchain"
        ) should be(Left(InvalidBlockNumber))
        log.warns.size should be(1)
      }
  }

  "Justification follow validation" should "return valid for proper justifications and failed otherwise" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        val v1     = ByteString.copyFromUtf8("Validator One")
        val v2     = ByteString.copyFromUtf8("Validator Two")
        val v1Bond = Bond(v1, 2)
        val v2Bond = Bond(v2, 3)
        val bonds  = Seq(v1Bond, v2Bond)

        val genesis = createBlock[Id](Seq(), ByteString.EMPTY, bonds)
        val b2 = createBlock[Id](
          Seq(genesis.blockHash),
          v2,
          bonds,
          HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
        )
        val b3 = createBlock[Id](
          Seq(genesis.blockHash),
          v1,
          bonds,
          HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
        )
        val b4 = createBlock[Id](
          Seq(b2.blockHash),
          v2,
          bonds,
          HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash)
        )
        val b5 = createBlock[Id](
          Seq(b2.blockHash),
          v1,
          bonds,
          HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash)
        )
        val b6 = createBlock[Id](
          Seq(b4.blockHash),
          v2,
          bonds,
          HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
        )
        val b7 = createBlock[Id](
          Seq(b4.blockHash),
          v1,
          Seq(),
          HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
        )
        val b8 = createBlock[Id](
          Seq(b7.blockHash),
          v1,
          bonds,
          HashMap(v1 -> b7.blockHash, v2 -> b4.blockHash)
        )

        (1 to 6).forall(
          i =>
            Validate
              .justificationFollows[Id](
                blockDagStorage.lookupByIdUnsafe(i),
                genesis,
                blockDagStorage.getRepresentation
              ) == Right(Valid)
        ) should be(true)

        Validate
          .justificationFollows[Id](
            blockDagStorage.lookupByIdUnsafe(7),
            genesis,
            blockDagStorage.getRepresentation
          ) == Left(InvalidFollows) should be(
          true
        )

        log.warns.size should be(1)
        log.warns.forall(_.contains("do not match the bonded validators")) should be(
          true
        )
      }
  }

  "Justification regression validation" should "return valid for proper justifications and justification regression detected otherwise" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        val validators = Vector(
          ByteString.copyFromUtf8("Validator 1"),
          ByteString.copyFromUtf8("Validator 2")
        )
        val bonds = validators.zipWithIndex.map {
          case (v, i) => Bond(v, 2L * i.toLong + 1L)
        }

        def latestMessages(messages: Seq[BlockMessage]): Map[Validator, BlockHash] =
          messages.map(b => b.sender -> b.blockHash).toMap

        def createValidatorBlock[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
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

        val b0 = createBlock[Id](Seq.empty, bonds = bonds)
        val b1 = createValidatorBlock[Id](Seq(b0), Seq(b0, b0), 0)
        val b2 = createValidatorBlock[Id](Seq(b1), Seq(b1, b0), 0)
        val b3 = createValidatorBlock[Id](Seq(b0), Seq(b2, b0), 1)
        val b4 = createValidatorBlock[Id](Seq(b3), Seq(b2, b3), 1)

        (0 to 4).forall(
          i =>
            Validate
              .justificationRegressions[Id](
                blockDagStorage.lookupByIdUnsafe(i),
                b0,
                blockDagStorage.getRepresentation
              ) == Right(Valid)
        ) should be(true)

        // The justification block for validator 0 should point to b2 or above.
        val justificationsWithRegression =
          Seq(
            Justification(validators(0), b1.blockHash),
            Justification(validators(1), b4.blockHash)
          )
        val blockWithJustificationRegression =
          BlockMessage()
            .withSender(validators(1))
            .withJustifications(justificationsWithRegression)
        Validate.justificationRegressions[Id](
          blockWithJustificationRegression,
          b0,
          blockDagStorage.getRepresentation
        ) should be(
          Left(JustificationRegression)
        )
        log.warns.size should be(1)
      }
  }

  "Bonds cache validation" should "succeed on a valid block and fail on modified bonds" in withStore {
    implicit blockStore =>
      withBlockDagStorage { implicit blockDagStorage =>
        val (_, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
        val bonds           = HashSetCasperTest.createBonds(validators)
        val genesis         = HashSetCasperTest.createGenesis(bonds)

        val storageDirectory  = Files.createTempDirectory(s"hash-set-casper-test-genesis")
        val storageSize: Long = 1024L * 1024
        val activeRuntime     = Runtime.create(storageDirectory, storageSize)
        val runtimeManager    = RuntimeManager.fromRuntime(activeRuntime)
        val _ = InterpreterUtil
          .validateBlockCheckpoint[Id](genesis, blockDagStorage.getRepresentation, runtimeManager)

        Validate.bondsCache[Id](genesis, runtimeManager) should be(Right(Valid))

        val modifiedBonds     = Seq.empty[Bond]
        val modifiedPostState = genesis.getBody.getState.withBonds(modifiedBonds)
        val modifiedBody      = genesis.getBody.withState(modifiedPostState)
        val modifiedGenesis   = genesis.withBody(modifiedBody)
        Validate.bondsCache[Id](modifiedGenesis, runtimeManager) should be(Left(InvalidBondsCache))

        activeRuntime.close().unsafeRunSync
      }
  }

  "Field format validation" should "succeed on a valid block and fail on empty fields" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        val (sk, pk) = Ed25519.newKeyPair
        val block    = HashSetCasperTest.createGenesis(Map(pk -> 1))
        val genesis =
          ProtoUtil
            .signBlock[Id](block, blockDagStorage.getRepresentation, pk, sk, "ed25519", "rchain")

        Validate.formatOfFields[Id](genesis) should be(true)
        Validate.formatOfFields[Id](genesis.withBlockHash(ByteString.EMPTY)) should be(false)
        Validate.formatOfFields[Id](genesis.clearHeader) should be(false)
        Validate.formatOfFields[Id](genesis.clearBody) should be(false)
        Validate.formatOfFields[Id](genesis.withSig(ByteString.EMPTY)) should be(false)
        Validate.formatOfFields[Id](genesis.withSigAlgorithm("")) should be(false)
        Validate.formatOfFields[Id](genesis.withShardId("")) should be(false)
        Validate.formatOfFields[Id](genesis.withBody(genesis.getBody.clearState)) should be(
          false
        )
        Validate.formatOfFields[Id](
          genesis.withHeader(genesis.header.get.withPostStateHash(ByteString.EMPTY))
        ) should be(false)
        Validate.formatOfFields[Id](
          genesis.withHeader(genesis.header.get.withDeploysHash(ByteString.EMPTY))
        ) should be(false)
        Validate.formatOfFields[Id](
          genesis.withBody(
            genesis.body.get
              .withDeploys(
                genesis.body.get.deploys.map(_.withLog(List(Event(EventInstance.Empty))))
              )
          )
        ) should be(false)
      }
  }

  "Block hash format validation" should "fail on invalid hash" in withStore { implicit blockStore =>
    withIndexedBlockDagStorage { implicit blockDagStorage =>
      val (sk, pk) = Ed25519.newKeyPair
      val block    = HashSetCasperTest.createGenesis(Map(pk -> 1))
      val genesis = ProtoUtil
        .signBlock[Id](block, blockDagStorage.getRepresentation, pk, sk, "ed25519", "rchain")
      Validate.blockHash[Id](genesis) should be(Right(Valid))
      Validate.blockHash[Id](
        genesis.withBlockHash(ByteString.copyFromUtf8("123"))
      ) should be(Left(InvalidBlockHash))
    }
  }

  "Block deploy count validation" should "fail on invalid number of deploys" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        val (sk, pk) = Ed25519.newKeyPair
        val block    = HashSetCasperTest.createGenesis(Map(pk -> 1))
        val genesis = ProtoUtil
          .signBlock[Id](block, blockDagStorage.getRepresentation, pk, sk, "ed25519", "rchain")
        Validate.deployCount[Id](genesis) should be(Right(Valid))
        Validate.deployCount[Id](
          genesis.withHeader(genesis.header.get.withDeployCount(100))
        ) should be(Left(InvalidDeployCount))
      }
  }

  "Block version validation" should "work" in withIndexedBlockDagStorage {
    implicit blockDagStorage =>
      val (sk, pk) = Ed25519.newKeyPair
      val block    = HashSetCasperTest.createGenesis(Map(pk -> 1))
      val genesis =
        ProtoUtil.signBlock(block, blockDagStorage.getRepresentation, pk, sk, "ed25519", "rchain")
      Validate.version[Id](genesis, -1) should be(false)
      Validate.version[Id](genesis, 1) should be(true)
  }
}
