package coop.rchain.casper.batch2

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagRepresentation}
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.Resources.mkTestRNodeStoreManager
import coop.rchain.casper.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.casper.util._
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.blockImplicits._
import coop.rchain.models.syntax._
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.rspace.syntax._
import coop.rchain.shared.Time
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import scala.collection.immutable.HashMap

class ValidateTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BlockGenerator
    with BlockDagStorageFixture {
  import InvalidBlock._
  import ValidBlock._

  implicit val log: LogStub[Task]     = new LogStub[Task]
  private val SHARD_ID                = "root-shard"
  implicit val span: Span[Task]       = NoopSpan[Task]()
  implicit val metrics: Metrics[Task] = new Metrics.MetricsNOP[Task]()
  implicit val s                      = Sync[Task]

  def mkCasperSnapshot[F[_]](dag: DagRepresentation) =
    CasperSnapshot(
      dag,
      ByteString.EMPTY,
      ByteString.EMPTY,
      IndexedSeq.empty,
      Set.empty,
      Set.empty,
      0,
      Map.empty,
      OnChainCasperState(
        CasperShardConf(0, "", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Map.empty,
        Seq.empty
      )
    )

  override def beforeEach(): Unit = {
    log.reset()
    timeEff.reset()
  }

  def createChain[F[_]: Sync: Time: BlockStore: BlockDagStorage](
      length: Int,
      bonds: Seq[Bond] = Seq.empty[Bond]
  ): F[Vector[BlockMessage]] =
    for {
      genesis <- createGenesis[F](bonds = bonds)
      blocks <- (1 to length).toList.foldLeftM(Vector(genesis)) {
                 case (chain, _) =>
                   createBlock[F](justifications = Seq(chain.last.blockHash), bonds = bonds)
                     .map(chain :+ _)
               }
    } yield blocks

  def createChainWithRoundRobinValidators[F[_]: Sync: Time: BlockStore: BlockDagStorage](
      length: Int,
      validatorLength: Int
  ): F[Vector[BlockMessage]] = {
    val validatorRoundRobinCycle = Stream.continually(0 until validatorLength).flatten
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
  )(implicit sk: PrivateKey, blockDagStorage: BlockDagStorage[Task]): Task[BlockMessage] = {
    val pk    = Secp256k1.toPublic(sk)
    val block = chain(i)
    for {
      dag              <- blockDagStorage.getRepresentation
      sender           = ByteString.copyFrom(pk.bytes)
      latestMessageOpt <- dag.latestMessage(sender)
      seqNum           = latestMessageOpt.fold(0L)(_.seqNum) + 1L
      result           = ValidatorIdentity(sk).signBlock(block.copy(seqNum = seqNum))
    } yield result
  }

  implicit class ChangeBlockNumber(b: BlockMessage) {
    def withBlockNumber(n: Long): BlockMessage =
      b.copy(blockNumber = n)
  }

  "Block signature validation" should "return false on unknown algorithms" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        chain            <- createChain[Task](2)
        unknownAlgorithm = "unknownAlgorithm"
        rsa              = "RSA"
        block0           = chain(0).copy(sigAlgorithm = unknownAlgorithm)
        block1           = chain(1).copy(sigAlgorithm = rsa)
        _                <- Validate.blockSignature[Task](block0) shouldBeF false
        _ = log.warns.last
          .contains(s"signature algorithm $unknownAlgorithm is unsupported") should be(
          true
        )
        _      <- Validate.blockSignature[Task](block1) shouldBeF false
        result = log.warns.last.contains(s"signature algorithm $rsa is unsupported") should be(true)
      } yield result
  }

  it should "return false on invalid secp256k1 signatures" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      implicit val (sk, _) = Secp256k1.newKeyPair
      for {
        chain        <- createChain[Task](6)
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
        _            <- blocks.existsM[Task](Validate.blockSignature[Task]) shouldBeF false
        _            = log.warns.size should be(blocks.length)
        result       = log.warns.forall(_.contains("signature is invalid")) should be(true)
      } yield result
  }

  it should "return true on valid secp256k1 signatures" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val n                 = 6
      implicit val (sk, pk) = Secp256k1.newKeyPair
      for {
        chain <- createChain[Task](n)
        condition <- (0 until n).toList.forallM[Task] { i =>
                      val chainWithSender = chain.map(_.copy(sender = pk.bytes.toByteString))
                      for {
                        block  <- signedBlock(chainWithSender, i)
                        result <- Validate.blockSignature[Task](block)
                      } yield result
                    }
        _      = condition should be(true)
        result = log.warns should be(Nil)
      } yield result
  }

  "Block number validation" should "only accept 0 as the number for a block with no parents" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        chain <- createChain[Task](1)
        block = chain(0)
        dag   <- blockDagStorage.getRepresentation
        _ <- Validate
              .blockNumber[Task](block.withBlockNumber(1), mkCasperSnapshot(dag)) shouldBeF Left(
              InvalidBlockNumber
            )
        _      <- Validate.blockNumber[Task](block, mkCasperSnapshot(dag)) shouldBeF Right(Valid)
        _      = log.warns.size should be(1)
        result = log.warns.head.contains("not zero, but block has no parents") should be(true)
      } yield result
  }

  it should "return false for non-sequential numbering" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        chain <- createChain[Task](2)
        block = chain(1)
        dag   <- blockDagStorage.getRepresentation
        _ <- Validate
              .blockNumber[Task](block.withBlockNumber(17), mkCasperSnapshot(dag)) shouldBeF Left(
              InvalidBlockNumber
            )
        _ <- Validate.blockNumber[Task](block, mkCasperSnapshot(dag)) shouldBeF Right(Valid)
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
        chain <- createChain[Task](n)
        dag   <- blockDagStorage.getRepresentation
        snap  = mkCasperSnapshot(dag)
        _ <- chain.forallM[Task] { b =>
              Validate.blockNumber[Task](b, snap).map(_ == Right(Valid))
            } shouldBeF true
        result = log.warns should be(Nil)
      } yield result
  }

  it should "correctly validate a multi-parent block where the parents have different block numbers" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      def createBlockWithNumber(
          validator: Validator,
          parents: Seq[BlockMessage] = Nil
      ): Task[BlockMessage] =
        for {
          block <- createValidatorBlock(parents, validator, Seq(), shardId = "")
        } yield block

      def genSender(id: Int) = List.fill(65)(id.toByte).toArray.toByteString

      for {
        genesis <- createChain[Task](8) // Note we need to create a useless chain to satisfy the assert in TopoSort
        v1      = genSender(1)
        v2      = genSender(2)
        b1      <- createBlockWithNumber(v1)
        b2      <- createBlockWithNumber(v2)
        b3      <- createBlockWithNumber(v2, Seq(b1, b2))
        dag     <- blockDagStorage.getRepresentation
        s1      <- Validate.blockNumber[Task](b3, mkCasperSnapshot(dag))
        _       = s1 shouldBe Right(Valid)
        s2      <- Validate.blockNumber[Task](b3.withBlockNumber(4), mkCasperSnapshot(dag))
        _       = s2 shouldBe Left(InvalidBlockNumber)
      } yield ()
  }

  "Future deploy validation" should "work" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        deploy     <- ConstructDeploy.basicProcessedDeploy[Task](0)
        deployData = deploy.deploy.data
        updatedDeployData = Signed(
          deployData.copy(validAfterBlockNumber = -1),
          Secp256k1,
          ConstructDeploy.defaultSec
        )
        block <- createGenesis[Task](
                  deploys = Seq(deploy.copy(deploy = updatedDeployData))
                )
        status <- Validate.futureTransaction[Task](block)
        _      = status should be(Right(Valid))
      } yield ()
  }

  "Future deploy validation" should "not accept blocks with a deploy for a future block number" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        deploy     <- ConstructDeploy.basicProcessedDeploy[Task](0)
        deployData = deploy.deploy.data
        updatedDeployData = Signed(
          deployData.copy(validAfterBlockNumber = Long.MaxValue),
          Secp256k1,
          ConstructDeploy.defaultSec
        )
        blockWithFutureDeploy <- createGenesis[Task](
                                  deploys = Seq(deploy.copy(deploy = updatedDeployData))
                                )
        status <- Validate.futureTransaction[Task](blockWithFutureDeploy)
        _      = status should be(Left(ContainsFutureDeploy))
      } yield ()
  }

  "Deploy expiration validation" should "work" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        deploy <- ConstructDeploy.basicProcessedDeploy[Task](0)
        block <- createGenesis[Task](
                  deploys = Seq(deploy)
                )
        status <- Validate.transactionExpiration[Task](block, expirationThreshold = 10)
        _      = status should be(Right(Valid))
      } yield ()
  }

  "Deploy expiration validation" should "not accept blocks with a deploy that is expired" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        deploy     <- ConstructDeploy.basicProcessedDeploy[Task](0)
        deployData = deploy.deploy.data
        updatedDeployData = Signed(
          deployData.copy(validAfterBlockNumber = Long.MinValue),
          Secp256k1,
          ConstructDeploy.defaultSec
        )
        blockWithExpiredDeploy <- createGenesis[Task](
                                   deploys = Seq(deploy.copy(deploy = updatedDeployData))
                                 )
        status <- Validate
                   .transactionExpiration[Task](blockWithExpiredDeploy, expirationThreshold = 10)
        _ = status should be(Left(ContainsExpiredDeploy))
      } yield ()
  }

  it should "return false for non-sequential numbering" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        chain <- createChain[Task](2)
        block = chain(1)
        _ <- Validate
              .sequenceNumber[Task](block.copy(seqNum = 1)) shouldBeF Left(
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
        chain <- createChainWithRoundRobinValidators[Task](n, validatorCount)
        _ <- chain.forallM[Task](
              block =>
                for {
                  result <- Validate.sequenceNumber[Task](block)
                } yield result == Right(Valid)
            ) shouldBeF true
        result = log.warns should be(Nil)
      } yield result
  }

  "Repeat deploy validation" should "return valid for empty blocks" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        chain  <- createChain[Task](2)
        block  = chain(0)
        block2 = chain(1)
        dag    <- blockDagStorage.getRepresentation
        _      <- Validate.repeatDeploy[Task](block, mkCasperSnapshot(dag), 50) shouldBeF Right(Valid)
        _      <- Validate.repeatDeploy[Task](block2, mkCasperSnapshot(dag), 50) shouldBeF Right(Valid)
      } yield ()
  }

  it should "not accept blocks with a repeated deploy" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        deploy  <- ConstructDeploy.basicProcessedDeploy[Task](0)
        genesis <- createGenesis[Task](deploys = Seq(deploy))
        block1  <- createBlock[Task](justifications = Seq(genesis.blockHash), deploys = Seq(deploy))
        dag     <- blockDagStorage.getRepresentation
        _ <- Validate.repeatDeploy[Task](block1, mkCasperSnapshot(dag), 50) shouldBeF Left(
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
        chain <- createChain[Task](2)
        block = chain(1)
        dag   <- blockDagStorage.getRepresentation

        (sk, pk) = Secp256k1.newKeyPair
        sender   = ByteString.copyFrom(pk.bytes)
        signedBlock = ValidatorIdentity(sk).signBlock(
          block.withBlockNumber(17).copy(seqNum = 1)
        )
        _ <- Validate.blockSummary[Task](
              signedBlock,
              mkCasperSnapshot(dag),
              "root",
              Int.MaxValue
            ) shouldBeF Left(InvalidBlockNumber)
        result = log.warns.size should be(1)
      } yield result
  }

  "Justification regression validation" should "return valid for proper justifications and justification regression detected otherwise" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v0 = generateValidator("Validator 1")
      val v1 = generateValidator("Validator 2")
      val bonds = List(v0, v1).zipWithIndex.map {
        case (v, i) => Bond(v, 2L * i.toLong + 1L)
      }

      for {
        b0 <- createGenesis[Task](bonds = bonds)
        b1 <- createValidatorBlock[Task](Seq(b0), v0, bonds, shardId = SHARD_ID)
        b2 <- createValidatorBlock[Task](Seq(b1, b0), v0, bonds, shardId = SHARD_ID)
        b3 <- createValidatorBlock[Task](Seq(b2, b0), v1, bonds, shardId = SHARD_ID)
        b4 <- createValidatorBlock[Task](Seq(b2, b3), v1, bonds, shardId = SHARD_ID)
        _ <- List(b0, b1, b2, b3, b4).forallM[Task](
              block =>
                for {
                  result <- Validate.justificationRegressions[Task](block)
                } yield result == Right(Valid)
            ) shouldBeF true
        // The justification block for validator 0 should point to b2 or above.
        justificationsWithRegression = Seq(b1.blockHash, b4.blockHash)
        blockWithJustificationRegression = getRandomBlock(
          setValidator = v1.some,
          setJustifications = justificationsWithRegression.some,
          hashF = (ProtoUtil.hashBlock _).some
        )
        result <- Validate.justificationRegressions[Task](blockWithJustificationRegression) shouldBeF
                   Left(JustificationRegression)
      } yield result
  }

  "Justification regression validation" should "return valid for regressive invalid blocks" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v0 = generateValidator("Validator 1")
      val v1 = generateValidator("Validator 2")
      val bonds = List(v0, v1).zipWithIndex.map {
        case (v, i) => Bond(v, 2L * i.toLong + 1L)
      }

      for {
        b0 <- createGenesis[Task](bonds = bonds)
        b1 <- createValidatorBlock[Task](Seq(b0), v0, bonds, 1, shardId = SHARD_ID)
        b2 <- createValidatorBlock[Task](Seq(b1, b0), v1, bonds, 1, shardId = SHARD_ID)
        b3 <- createValidatorBlock[Task](Seq(b1, b2), v0, bonds, 2, shardId = SHARD_ID)
        b4 <- createValidatorBlock[Task](Seq(b3, b2), v1, bonds, 2, shardId = SHARD_ID)
        b5 <- createValidatorBlock[Task](
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
        _ <- Validate.justificationRegressions[Task](blockWithInvalidJustification) shouldBeF
              Right(Valid)
      } yield ()
  }

  "Bonds cache validation" should "succeed on a valid block and fail on modified bonds" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val genesis = GenesisBuilder.createGenesis()

      val storageDirectory = Files.createTempDirectory(s"hash-set-casper-test-genesis-")

      for {
        _      <- blockDagStorage.insert(genesis, false, approved = true)
        kvm    <- mkTestRNodeStoreManager[Task](storageDirectory)
        rStore <- kvm.rSpaceStores
        mStore <- RuntimeManager.mergeableStore(kvm)
        runtimeManager <- RuntimeManager[Task](
                           rStore,
                           mStore,
                           Genesis.NonNegativeMergeableTagName,
                           RuntimeManager.noOpExecutionTracker[Task]
                         )
        dag <- blockDagStorage.getRepresentation
        _ <- InterpreterUtil
              .validateBlockCheckpoint[Task](genesis, mkCasperSnapshot(dag), runtimeManager)
        _                 <- Validate.bondsCache[Task](genesis, runtimeManager) shouldBeF Right(Valid)
        modifiedBonds     = Seq.empty[Bond]
        modifiedPostState = genesis.body.state.copy(bonds = modifiedBonds.toList)
        modifiedBody      = genesis.body.copy(state = modifiedPostState)
        modifiedGenesis   = genesis.copy(body = modifiedBody)
        result <- Validate.bondsCache[Task](modifiedGenesis, runtimeManager) shouldBeF Left(
                   InvalidBondsCache
                 )
      } yield result
  }

  "Field format validation" should "succeed on a valid block and fail on empty fields" in withStorage {
    _ => implicit blockDagStorage =>
      val context  = buildGenesis()
      val (sk, pk) = context.validatorKeyPairs.head
      for {
        _                <- blockDagStorage.insert(genesis, false, approved = true)
        dag              <- blockDagStorage.getRepresentation
        sender           = ByteString.copyFrom(pk.bytes)
        latestMessageOpt <- dag.latestMessage(sender)
        seqNum           = latestMessageOpt.fold(0L)(_.seqNum) + 1L
        genesis = ValidatorIdentity(sk)
          .signBlock(context.genesisBlock.copy(seqNum = seqNum))
        _ <- Validate.formatOfFields[Task](genesis) shouldBeF true
        _ <- Validate.formatOfFields[Task](genesis.copy(blockHash = ByteString.EMPTY)) shouldBeF false
        _ <- Validate.formatOfFields[Task](genesis.copy(sig = ByteString.EMPTY)) shouldBeF false
        _ <- Validate.formatOfFields[Task](genesis.copy(sigAlgorithm = "")) shouldBeF false
        _ <- Validate.formatOfFields[Task](genesis.copy(shardId = "")) shouldBeF false
        _ <- Validate.formatOfFields[Task](
              genesis.copy(
                body = genesis.body
                  .copy(state = genesis.body.state.copy(postStateHash = ByteString.EMPTY))
              )
            ) shouldBeF false
      } yield ()
  }

  "Block hash format validation" should "fail on invalid hash" in withStorage {
    _ => implicit blockDagStorage =>
      val context  = buildGenesis()
      val (sk, pk) = context.validatorKeyPairs.head
      val sender   = ByteString.copyFrom(pk.bytes)
      for {
        _                <- blockDagStorage.insert(genesis, false, approved = true)
        dag              <- blockDagStorage.getRepresentation
        latestMessageOpt <- dag.latestMessage(sender)
        seqNum           = latestMessageOpt.fold(0L)(_.seqNum) + 1L
        genesis = ValidatorIdentity(sk)
          .signBlock(context.genesisBlock.copy(seqNum = seqNum))
        _ <- Validate.blockHash[Task](genesis) shouldBeF true
        result <- Validate.blockHash[Task](
                   genesis.copy(blockHash = ByteString.copyFromUtf8("123"))
                 ) shouldBeF false
      } yield result
  }

  "Block version validation" should "work" in withStorage { _ => implicit blockDagStorage =>
    val context  = buildGenesis()
    val (sk, pk) = context.validatorKeyPairs.head
    val sender   = ByteString.copyFrom(pk.bytes)
    for {
      _                <- blockDagStorage.insert(genesis, false, approved = true)
      dag              <- blockDagStorage.getRepresentation
      latestMessageOpt <- dag.latestMessage(sender)
      seqNum           = latestMessageOpt.fold(0L)(_.seqNum) + 1L
      genesis = ValidatorIdentity(sk).signBlock(
        context.genesisBlock.copy(seqNum = seqNum)
      )
      _      <- Validate.version[Task](genesis, -1) shouldBeF false
      result <- Validate.version[Task](genesis, 1) shouldBeF true
    } yield result
  }

}
