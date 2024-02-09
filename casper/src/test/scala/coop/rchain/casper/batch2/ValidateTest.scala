package coop.rchain.casper.batch2

import cats.effect.unsafe.implicits.global
import cats.effect.{Async, IO, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper._
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.Resources.mkTestRNodeStoreManager
import coop.rchain.casper.rholang.{BlockRandomSeed, InterpreterUtil, RuntimeManager}
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.casper.util._
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockVersion
import coop.rchain.models.Validator.Validator
import coop.rchain.models.blockImplicits._
import coop.rchain.models.syntax._
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.rspace.syntax._
import coop.rchain.shared.Log
import coop.rchain.shared.scalatestcontrib._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.nio.file.Files
import scala.collection.immutable.HashMap

class ValidateTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BlockGenerator
    with BlockDagStorageFixture
    with ScalaCheckPropertyChecks {
  import InvalidBlock._
  import ValidBlock._

  implicit val log: LogStub[IO]     = new LogStub
  private val SHARD_ID              = "root-shard"
  implicit val span: Span[IO]       = NoopSpan[IO]()
  implicit val metrics: Metrics[IO] = new Metrics.MetricsNOP[IO]()
  implicit val s                    = Sync[IO]

  override def beforeEach(): Unit = {
    log.reset()
    timeEff.reset()
  }

  def createChain[F[_]: Async: BlockStore: BlockDagStorage](
      length: Int,
      bonds: Map[Validator, Long] = Map.empty
  ): F[Vector[BlockMessage]] =
    for {
      genesis <- createGenesis[F](bonds = bonds)
      blocks <- (1 to length).toList.foldLeftM(Vector(genesis)) {
                 case (chain, _) =>
                   createBlock[F](justifications = Seq(chain.last.blockHash), bonds = bonds)
                     .map(chain :+ _)
               }
    } yield blocks

  def createChainWithRoundRobinValidators[F[_]: Async: BlockStore: BlockDagStorage](
      length: Int,
      validatorLength: Int
  ): F[Vector[BlockMessage]] = {
    val validatorRoundRobinCycle = LazyList.continually(0 until validatorLength).flatten
    val validators               = List.fill(validatorLength)(generateValidator())
    (0 until length).toList
      .zip(validatorRoundRobinCycle)
      .foldLeft(
        for {
          genesis             <- createGenesis[F]()
          emptyLatestMessages <- HashMap.empty[Validator, BlockHash].pure[F]
        } yield (Vector(genesis), genesis, emptyLatestMessages)
      ) {
        case (acc, (_, validatorNum)) =>
          val creator = validators(validatorNum)
          for {
            unwrappedAcc                   <- acc
            (chain, block, latestMessages) = unwrappedAcc
            bnext <- createBlock[F](
                      creator = creator,
                      justifications = latestMessages.values.toSeq
                    )
            latestMessagesNext = latestMessages.updated(bnext.sender, bnext.blockHash)
          } yield (chain :+ bnext, bnext, latestMessagesNext)
      }
      .map(_._1)
  }

  def signedBlock(
      chain: Vector[BlockMessage],
      i: Int
  )(implicit sk: PrivateKey, blockDagStorage: BlockDagStorage[IO]): IO[BlockMessage] = {
    val pk    = Secp256k1.toPublic(sk)
    val block = chain(i)
    for {
      dag    <- blockDagStorage.getRepresentation
      sender = ByteString.copyFrom(pk.bytes)
      seqNum = getLatestSeqNum(sender, dag) + 1L
      result = ValidatorIdentity(sk).signBlock(block.copy(seqNum = seqNum))
    } yield result
  }

  "Block signature validation" should "return false on unknown algorithms" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        chain            <- createChain[IO](2)
        unknownAlgorithm = "unknownAlgorithm"
        rsa              = "RSA"
        block0           = chain(0).copy(sigAlgorithm = unknownAlgorithm)
        block1           = chain(1).copy(sigAlgorithm = rsa)
        _                <- Validate.blockSignature[IO](block0) shouldBeF false
        _ = log.warns.last
          .contains(s"signature algorithm $unknownAlgorithm is unsupported") should be(
          true
        )
        _      <- Validate.blockSignature[IO](block1) shouldBeF false
        result = log.warns.last.contains(s"signature algorithm $rsa is unsupported") should be(true)
      } yield result
  }

  it should "return false on invalid secp256k1 signatures" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      implicit val (sk, _) = Secp256k1.newKeyPair
      for {
        chain        <- createChain[IO](6)
        (_, wrongPk) = Secp256k1.newKeyPair
        empty        = ByteString.EMPTY
        invalidKey   = "abcdef1234567890".unsafeHexToByteString
        block0       <- signedBlock(chain, 0).map(_.copy(sender = empty))
        block1       <- signedBlock(chain, 1).map(_.copy(sender = invalidKey))
        block2       <- signedBlock(chain, 2).map(_.copy(sender = ByteString.copyFrom(wrongPk.bytes)))
        block3       <- signedBlock(chain, 3).map(_.copy(sig = empty))
        block4       <- signedBlock(chain, 4).map(_.copy(sig = invalidKey))
        block5       <- signedBlock(chain, 5).map(_.copy(sig = block0.sig)) //wrong sig
        blocks       = Vector(block0, block1, block2, block3, block4, block5)
        _            <- blocks.existsM[IO](Validate.blockSignature[IO]) shouldBeF false
        _            = log.warns.size should be(blocks.length)
        result       = log.warns.forall(_.contains("signature is invalid")) should be(true)
      } yield result
  }

  it should "return true on valid secp256k1 signatures" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val n                 = 6
      implicit val (sk, pk) = Secp256k1.newKeyPair
      for {
        chain <- createChain[IO](n)
        condition <- (0 until n).toList.forallM[IO] { i =>
                      val chainWithSender = chain.map(_.copy(sender = pk.bytes.toByteString))
                      for {
                        block  <- signedBlock(chainWithSender, i)
                        result <- Validate.blockSignature[IO](block)
                      } yield result
                    }
        _      = condition should be(true)
        result = log.warns should be(Nil)
      } yield result
  }

  "Block number validation" should "only accept 0 as the number for a block with no parents" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        chain <- createChain[IO](1)
        block = chain(0)
        _ <- Validate
              .blockNumber[IO](block.copy(blockNumber = 1)) shouldBeF Left(
              InvalidBlockNumber
            )
        _      <- Validate.blockNumber[IO](block) shouldBeF Right(Valid)
        _      = log.warns.size should be(1)
        result = log.warns.head.contains("not zero, but block has no parents") should be(true)
      } yield result
  }

  it should "return false for non-sequential numbering" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        chain <- createChain[IO](2)
        block = chain(1)
        _ <- Validate
              .blockNumber[IO](block.copy(blockNumber = 17)) shouldBeF Left(
              InvalidBlockNumber
            )
        _ <- Validate.blockNumber[IO](block) shouldBeF Right(Valid)
        _ = log.warns.size should be(1)
        result = log.warns.head.contains("is not one more than maximum parent number") should be(
          true
        )
      } yield result
  }

  it should "return true for sequential numbering" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val n = 6
      for {
        chain <- createChain[IO](n)
        _ <- chain.forallM[IO] { b =>
              Validate.blockNumber[IO](b).map(_ == Right(Valid))
            } shouldBeF true
        result = log.warns should be(Nil)
      } yield result
  }

  it should "correctly validate a multi-parent block where the parents have different block numbers" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      def createBlockWithNumber(
          validator: Validator,
          parents: Seq[BlockMessage] = Nil
      ): IO[BlockMessage] =
        for {
          block <- createValidatorBlock(parents, validator, Map(), shardId = "")
        } yield block

      def genSender(id: Int) = List.fill(65)(id.toByte).toArray.toByteString

      for {
        genesis <- createChain[IO](8) // Note we need to create a useless chain to satisfy the assert in TopoSort
        v1      = genSender(1)
        v2      = genSender(2)
        b1      <- createBlockWithNumber(v1)
        b2      <- createBlockWithNumber(v2)
        b3      <- createBlockWithNumber(v2, Seq(b1, b2))
        s1      <- Validate.blockNumber[IO](b3)
        _       = s1 shouldBe Right(Valid)
        s2      <- Validate.blockNumber[IO](b3.copy(blockNumber = 4))
        _       = s2 shouldBe Left(InvalidBlockNumber)
      } yield ()
  }

  "Future deploy validation" should "work" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        deploy     <- ConstructDeploy.basicProcessedDeploy[IO](0)
        deployData = deploy.deploy.data
        updatedDeployData = Signed(
          deployData.copy(validAfterBlockNumber = -1),
          Secp256k1,
          ConstructDeploy.defaultSec
        )
        block <- createGenesis[IO](
                  deploys = Seq(deploy.copy(deploy = updatedDeployData))
                )
        status <- Validate.futureTransaction[IO](block)
        _      = status should be(Right(Valid))
      } yield ()
  }

  "Future deploy validation" should "not accept blocks with a deploy for a future block number" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        deploy     <- ConstructDeploy.basicProcessedDeploy[IO](0)
        deployData = deploy.deploy.data
        updatedDeployData = Signed(
          deployData.copy(validAfterBlockNumber = Long.MaxValue),
          Secp256k1,
          ConstructDeploy.defaultSec
        )
        blockWithFutureDeploy <- createGenesis[IO](
                                  deploys = Seq(deploy.copy(deploy = updatedDeployData))
                                )
        status <- Validate.futureTransaction[IO](blockWithFutureDeploy)
        _      = status should be(Left(ContainsFutureDeploy))
      } yield ()
  }

  "Deploy expiration validation" should "work" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        deploy <- ConstructDeploy.basicProcessedDeploy[IO](0)
        block <- createGenesis[IO](
                  deploys = Seq(deploy)
                )
        status <- Validate.transactionExpiration[IO](block, expirationThreshold = 10)
        _      = status should be(Right(Valid))
      } yield ()
  }

  "Deploy expiration validation" should "not accept blocks with a deploy that is expired" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        deploy     <- ConstructDeploy.basicProcessedDeploy[IO](0)
        deployData = deploy.deploy.data
        updatedDeployData = Signed(
          deployData.copy(validAfterBlockNumber = Long.MinValue),
          Secp256k1,
          ConstructDeploy.defaultSec
        )
        blockWithExpiredDeploy <- createGenesis[IO](
                                   deploys = Seq(deploy.copy(deploy = updatedDeployData))
                                 )
        status <- Validate
                   .transactionExpiration[IO](blockWithExpiredDeploy, expirationThreshold = 10)
        _ = status should be(Left(ContainsExpiredDeploy))
      } yield ()
  }

  it should "return false for non-sequential numbering" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        chain <- createChain[IO](2)
        block = chain(1)
        _ <- Validate
              .sequenceNumber[IO](block.copy(seqNum = 1)) shouldBeF Left(
              InvalidSequenceNumber
            )
        result = log.warns.size should be(1)
      } yield result
  }

  it should "return true for sequential numbering" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val n              = 20
      val validatorCount = 3
      for {
        chain <- createChainWithRoundRobinValidators[IO](n, validatorCount)
        _ <- chain.forallM[IO](
              block =>
                for {
                  result <- Validate.sequenceNumber[IO](block)
                } yield result == Right(Valid)
            ) shouldBeF true
        result = log.warns should be(Nil)
      } yield result
  }

  "Repeat deploy validation" should "return valid for empty blocks" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        chain  <- createChain[IO](2)
        block  = chain(0)
        block2 = chain(1)
        _      <- Validate.repeatDeploy[IO](block, 50) shouldBeF Right(Valid)
        _      <- Validate.repeatDeploy[IO](block2, 50) shouldBeF Right(Valid)
      } yield ()
  }

  it should "not accept blocks with a repeated deploy" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        deploy  <- ConstructDeploy.basicProcessedDeploy[IO](0)
        genesis <- createGenesis[IO](deploys = Seq(deploy))
        block1  <- createBlock[IO](justifications = Seq(genesis.blockHash), deploys = Seq(deploy))
        _ <- Validate.repeatDeploy[IO](block1, 50) shouldBeF Left(
              InvalidRepeatDeploy
            )
      } yield ()
  }

  val genesisContext = GenesisBuilder.buildGenesis()
  val genesis        = genesisContext.genesisBlock

  // Creates a block with an invalid block number and sequence number
  "Block summary validation" should "short circuit after first invalidity" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        chain <- createChain[IO](2)
        block = chain(1)

        (sk, pk) = Secp256k1.newKeyPair
        signedBlock = ValidatorIdentity(sk).signBlock(
          block.copy(blockNumber = 17).copy(seqNum = 1)
        )
        _ <- Validate.blockSummary[IO](
              signedBlock,
              "root",
              Int.MaxValue
            ) shouldBeF Left(InvalidSequenceNumber)
        result = log.warns.size should be(1)
      } yield result
  }

  "Justification regression validation" should "return valid for proper justifications and justification regression detected otherwise" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v0 = generateValidator("Validator 1")
      val v1 = generateValidator("Validator 2")
      val bonds = List(v0, v1).zipWithIndex.map {
        case (v, i) => (v, 2L * i.toLong + 1L)
      }.toMap

      for {
        b0 <- createGenesis[IO](bonds = bonds)
        b1 <- createValidatorBlock[IO](Seq(b0), v0, bonds, shardId = SHARD_ID)
        b2 <- createValidatorBlock[IO](Seq(b1, b0), v0, bonds, shardId = SHARD_ID)
        b3 <- createValidatorBlock[IO](Seq(b2, b0), v1, bonds, shardId = SHARD_ID)
        b4 <- createValidatorBlock[IO](Seq(b2, b3), v1, bonds, shardId = SHARD_ID)
        _ <- List(b0, b1, b2, b3, b4).forallM[IO](
              block =>
                for {
                  result <- Validate.justificationRegressions[IO](block)
                } yield result == Right(Valid)
            ) shouldBeF true
        // The justification block for validator 0 should point to b2 or above.
        justificationsWithRegression = Seq(b1.blockHash, b4.blockHash)
        blockWithJustificationRegression = getRandomBlock(
          setValidator = v1.some,
          setJustifications = justificationsWithRegression.some,
          hashF = (ProtoUtil.hashBlock _).some
        )
        result <- Validate.justificationRegressions[IO](blockWithJustificationRegression) shouldBeF
                   Left(JustificationRegression)
      } yield result
  }

  "Justification regression validation" should "return valid for regressive invalid blocks" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v0 = generateValidator("Validator 1")
      val v1 = generateValidator("Validator 2")
      val bonds = List(v0, v1).zipWithIndex.map {
        case (v, i) => (v, 2L * i.toLong + 1L)
      }.toMap

      for {
        b0 <- createGenesis[IO](bonds = bonds)
        b1 <- createValidatorBlock[IO](Seq(b0), v0, bonds, 1, shardId = SHARD_ID)
        b2 <- createValidatorBlock[IO](Seq(b1, b0), v1, bonds, 1, shardId = SHARD_ID)
        b3 <- createValidatorBlock[IO](Seq(b1, b2), v0, bonds, 2, shardId = SHARD_ID)
        b4 <- createValidatorBlock[IO](Seq(b3, b2), v1, bonds, 2, shardId = SHARD_ID)
        b5 <- createValidatorBlock[IO](
               Seq(b3, b4),
               v0,
               bonds,
               1,
               invalid = true,
               shardId = SHARD_ID
             )

        justificationsWithInvalidBlock = Seq(b5.blockHash, b4.blockHash)
        blockWithInvalidJustification = getRandomBlock(
          setValidator = v1.some,
          setJustifications = justificationsWithInvalidBlock.some
        )
        _ <- Validate.justificationRegressions[IO](blockWithInvalidJustification) shouldBeF
              Right(Valid)
      } yield ()
  }

  "Bonds cache validation" should "succeed on a valid block and fail on modified bonds" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val genesis = GenesisBuilder.createGenesis()

      val storageDirectory = Files.createTempDirectory(s"hash-set-casper-test-genesis-")

      for {
        kvm    <- mkTestRNodeStoreManager[IO](storageDirectory)
        rStore <- kvm.rSpaceStores
        mStore <- RuntimeManager.mergeableStore(kvm)
        runtimeManager <- RuntimeManager[IO](
                           rStore,
                           mStore,
                           BlockRandomSeed.nonNegativeMergeableTagName(
                             genesis.shardId
                           ),
                           RuntimeManager.noOpExecutionTracker[IO]
                         )
        result <- {
          implicit val rm = runtimeManager
          for {
            _               <- InterpreterUtil.validateBlockCheckpointLegacy[IO](genesis)
            _               <- Validate.bondsCache[IO](genesis) shouldBeF Right(Valid)
            modifiedBonds   = Map.empty[Validator, Long]
            modifiedGenesis = genesis.copy(bonds = modifiedBonds)
            result          <- Validate.bondsCache[IO](modifiedGenesis) shouldBeF Left(InvalidBondsCache)
          } yield result
        }
      } yield result
  }

  "Field format validation" should "succeed on a valid block and fail on empty fields" in withStorage {
    _ => implicit blockDagStorage =>
      val context  = buildGenesis()
      val (sk, pk) = context.validatorKeyPairs.head
      for {
        dag    <- blockDagStorage.getRepresentation
        sender = ByteString.copyFrom(pk.bytes)
        seqNum = getLatestSeqNum(sender, dag) + 1L
        genesis = ValidatorIdentity(sk)
          .signBlock(context.genesisBlock.copy(seqNum = seqNum))
        _ <- Validate.formatOfFields[IO](genesis) shouldBeF true
        _ <- Validate.formatOfFields[IO](genesis.copy(blockHash = ByteString.EMPTY)) shouldBeF false
        _ <- Validate.formatOfFields[IO](genesis.copy(sig = ByteString.EMPTY)) shouldBeF false
        _ <- Validate.formatOfFields[IO](genesis.copy(sigAlgorithm = "")) shouldBeF false
//        _ <- Validate.formatOfFields[IO](genesis.copy(shardId = "")) shouldBeF false
        _ <- Validate.formatOfFields[IO](
              genesis.copy(postStateHash = ByteString.EMPTY)
            ) shouldBeF false
      } yield ()
  }

  "Block hash format validation" should "fail on invalid hash" in {
    implicit val aBlock = arbBlockMessage

    forAll { (block: BlockMessage) =>
      val hash           = ProtoUtil.hashBlock(block)
      val blockValidHash = block.copy(blockHash = hash)

      // Test valid block hash
      val hashValid = Validate.blockHash[IO](blockValidHash).unsafeRunSync()

      hashValid shouldBe true

      val blockInValidHash = block.copy(blockHash = ByteString.copyFromUtf8("123"))

      // Test invalid block hash
      val hashInValid = Validate.blockHash[IO](blockInValidHash).unsafeRunSync()

      hashInValid shouldBe false
    }
  }

  "Block version validation" should "allow supported versions" in {
    implicit val aBlock = arbBlockMessage

    forAll { (block: BlockMessage, version: Int) =>
      val blockWithVersion = block.copy(version = version)

      // Expected one of hard-coded block versions supported by this version of RNode software
      val expectedValid = BlockVersion.Supported.contains(version)
      // Actual validation
      val actualValid = Validate.version[IO](blockWithVersion).unsafeRunSync()

      actualValid shouldBe expectedValid
    }
  }

}
