package coop.rchain.casper.batch2

import cats.Monad
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.IndexedBlockDagStorage
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.helper.{
  BlockDagStorageFixture,
  BlockGenerator,
  UnlimitedParentsEstimatorFixture
}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.casper.util._
import coop.rchain.casper.util.rholang.Resources.mkTestRNodeStoreManager
import coop.rchain.casper.util.rholang.{InterpreterUtil, RuntimeManager}
import coop.rchain.casper.{InvalidBlock, ValidBlock, Validate, ValidatorIdentity}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.blockImplicits._
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.shared.Time
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest._

import java.nio.file.Files
import scala.collection.immutable.HashMap

class ValidateTest
    extends FlatSpec
    with Matchers
    with BeforeAndAfterEach
    with BlockGenerator
    with BlockDagStorageFixture
    with UnlimitedParentsEstimatorFixture {
  import InvalidBlock._
  import ValidBlock._

  implicit override val log: LogStub[Task] = new LogStub[Task]

  override def beforeEach(): Unit = {
    log.reset()
    timeEff.reset()
  }

  def createChain[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
      length: Int,
      bonds: Seq[Bond] = Seq.empty[Bond]
  ): F[BlockMessage] =
    for {
      genesis <- createGenesis[F](bonds = bonds)
      _ <- (1 to length).toList.foldLeftM(genesis) {
            case (block, _) =>
              createBlock[F](Seq(block.blockHash), genesis, bonds = bonds)
          }
    } yield genesis

  def createChainWithRoundRobinValidators[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
      length: Int,
      validatorLength: Int
  ): F[BlockMessage] = {
    val validatorRoundRobinCycle = Stream.continually(0 until validatorLength).flatten
    val validators               = List.fill(validatorLength)(generateValidator())
    (0 until length).toList
      .zip(validatorRoundRobinCycle)
      .foldLeft(
        for {
          genesis             <- createGenesis[F]()
          emptyLatestMessages <- HashMap.empty[Validator, BlockHash].pure[F]
        } yield (genesis, genesis, emptyLatestMessages)
      ) {
        case (acc, (_, validatorNum)) =>
          val creator = validators(validatorNum)
          for {
            unwrappedAcc                     <- acc
            (genesis, block, latestMessages) = unwrappedAcc
            bnext <- createBlock[F](
                      Seq(block.blockHash),
                      genesis,
                      creator = creator,
                      justifications = latestMessages
                    )
            latestMessagesNext = latestMessages.updated(bnext.sender, bnext.blockHash)
          } yield (genesis, bnext, latestMessagesNext)
      }
      .map(_._1)
  }

  def signedBlock(
      i: Int
  )(implicit sk: PrivateKey, blockDagStorage: IndexedBlockDagStorage[Task]): Task[BlockMessage] = {
    val pk = Secp256k1.toPublic(sk)
    for {
      block            <- blockDagStorage.lookupByIdUnsafe(i)
      dag              <- blockDagStorage.getRepresentation
      sender           = ByteString.copyFrom(pk.bytes)
      latestMessageOpt <- dag.latestMessage(sender)
      seqNum           = latestMessageOpt.fold(0)(_.seqNum) + 1
      result           = ValidatorIdentity(sk).signBlock(block.copy(seqNum = seqNum))
    } yield result
  }

  implicit class ChangeBlockNumber(b: BlockMessage) {
    def withBlockNumber(n: Long): BlockMessage = {
      val newState = b.body.state.copy(blockNumber = n)
      b.copy(body = b.body.copy(state = newState))
    }
  }

  "Block signature validation" should "return false on unknown algorithms" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        _                <- createChain[Task](2)
        unknownAlgorithm = "unknownAlgorithm"
        rsa              = "RSA"
        block0           <- blockDagStorage.lookupByIdUnsafe(0).map(_.copy(sigAlgorithm = unknownAlgorithm))
        block1           <- blockDagStorage.lookupByIdUnsafe(1).map(_.copy(sigAlgorithm = rsa))
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
        _            <- createChain[Task](6)
        (_, wrongPk) = Secp256k1.newKeyPair
        empty        = ByteString.EMPTY
        invalidKey   = ByteString.copyFrom(Base16.unsafeDecode("abcdef1234567890"))
        block0       <- signedBlock(0).map(_.copy(sender = empty))
        block1       <- signedBlock(1).map(_.copy(sender = invalidKey))
        block2       <- signedBlock(2).map(_.copy(sender = ByteString.copyFrom(wrongPk.bytes)))
        block3       <- signedBlock(3).map(_.copy(sig = empty))
        block4       <- signedBlock(4).map(_.copy(sig = invalidKey))
        block5       <- signedBlock(5).map(_.copy(sig = block0.sig)) //wrong sig
        blocks       = Vector(block0, block1, block2, block3, block4, block5)
        _            <- blocks.existsM[Task](Validate.blockSignature[Task]) shouldBeF false
        _            = log.warns.size should be(blocks.length)
        result       = log.warns.forall(_.contains("signature is invalid")) should be(true)
      } yield result
  }

  it should "return true on valid secp256k1 signatures" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val n                = 6
      implicit val (sk, _) = Secp256k1.newKeyPair
      for {
        _ <- createChain[Task](n)
        condition <- (0 until n).toList.forallM[Task] { i =>
                      for {
                        block  <- signedBlock(i)
                        result <- Validate.blockSignature[Task](block)
                      } yield result
                    }
        _      = condition should be(true)
        result = log.warns should be(Nil)
      } yield result
  }

  "Timestamp validation" should "not accept blocks with future time" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        _                       <- createChain[Task](1)
        block                   <- blockDagStorage.lookupByIdUnsafe(0)
        modifiedTimestampHeader = block.header.copy(timestamp = 99999999)
        dag                     <- blockDagStorage.getRepresentation
        _ <- Validate.timestamp[Task](
              block.copy(header = modifiedTimestampHeader),
              dag
            ) shouldBeF InvalidTimestamp.asLeft[ValidBlock]
        _      <- Validate.timestamp[Task](block, dag) shouldBeF Valid.asRight[InvalidBlock]
        _      = log.warns.size should be(1)
        result = log.warns.head.contains("block timestamp") should be(true)
      } yield result
  }

  it should "not accept blocks that were published before parent time" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        _                       <- createChain[Task](2)
        block                   <- blockDagStorage.lookupByIdUnsafe(1)
        modifiedTimestampHeader = block.header.copy(timestamp = -1)
        dag                     <- blockDagStorage.getRepresentation
        _ <- Validate.timestamp[Task](
              block.copy(header = modifiedTimestampHeader),
              dag
            ) shouldBeF Left(InvalidTimestamp)
        _      <- Validate.timestamp[Task](block, dag) shouldBeF Right(Valid)
        _      = log.warns.size should be(1)
        result = log.warns.head.contains("block timestamp") should be(true)
      } yield result
  }

  "Block number validation" should "only accept 0 as the number for a block with no parents" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        _     <- createChain[Task](1)
        block <- blockDagStorage.lookupByIdUnsafe(0)
        dag   <- blockDagStorage.getRepresentation
        _ <- Validate.blockNumber[Task](block.withBlockNumber(1), dag) shouldBeF Left(
              InvalidBlockNumber
            )
        _      <- Validate.blockNumber[Task](block, dag) shouldBeF Right(Valid)
        _      = log.warns.size should be(1)
        result = log.warns.head.contains("not zero, but block has no parents") should be(true)
      } yield result
  }

  it should "return false for non-sequential numbering" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        _     <- createChain[Task](2)
        block <- blockDagStorage.lookupByIdUnsafe(1)
        dag   <- blockDagStorage.getRepresentation
        _ <- Validate.blockNumber[Task](block.withBlockNumber(17), dag) shouldBeF Left(
              InvalidBlockNumber
            )
        _ <- Validate.blockNumber[Task](block, dag) shouldBeF Right(Valid)
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
        _   <- createChain[Task](n)
        dag <- blockDagStorage.getRepresentation
        a0  <- blockDagStorage.lookupByIdUnsafe(0) >>= (b => Validate.blockNumber[Task](b, dag))
        a1  <- blockDagStorage.lookupByIdUnsafe(1) >>= (b => Validate.blockNumber[Task](b, dag))
        a2  <- blockDagStorage.lookupByIdUnsafe(2) >>= (b => Validate.blockNumber[Task](b, dag))
        a3  <- blockDagStorage.lookupByIdUnsafe(3) >>= (b => Validate.blockNumber[Task](b, dag))
        a4  <- blockDagStorage.lookupByIdUnsafe(4) >>= (b => Validate.blockNumber[Task](b, dag))
        a5  <- blockDagStorage.lookupByIdUnsafe(5) >>= (b => Validate.blockNumber[Task](b, dag))
        _ <- (0 until n).toList.forallM[Task] { i =>
              (blockDagStorage.lookupByIdUnsafe(i) >>= (b => Validate.blockNumber[Task](b, dag)))
                .map(_ == Right(Valid))
            } shouldBeF true
        result = log.warns should be(Nil)
      } yield result
  }

  it should "correctly validate a multi-parent block where the parents have different block numbers" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      def createBlockWithNumber(
          n: Long,
          genesis: BlockMessage,
          parentHashes: Seq[ByteString] = Nil
      ): Task[BlockMessage] =
        for {
          timestamp <- Time[Task].currentMillis
          block = getRandomBlock(
            setBlockNumber = n.some,
            setTimestamp = timestamp.some,
            setParentsHashList = parentHashes.some,
            hashF = (ProtoUtil.hashUnsignedBlock _).some
          )

          _ <- blockStore.put(block.blockHash, block)
          _ <- blockDagStorage.insert(block, false)
        } yield block

      for {
        genesis <- createChain[Task](8) // Note we need to create a useless chain to satisfy the assert in TopoSort
        b1      <- createBlockWithNumber(3, genesis)
        b2      <- createBlockWithNumber(7, genesis)
        b3      <- createBlockWithNumber(8, genesis, Seq(b1.blockHash, b2.blockHash))
        dag     <- blockDagStorage.getRepresentation
        s1      <- Validate.blockNumber[Task](b3, dag)
        _       = s1 shouldBe Right(Valid)
        s2      <- Validate.blockNumber[Task](b3.withBlockNumber(4), dag)
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

  "Sequence number validation" should "only accept 0 as the number for a block with no parents" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        _     <- createChain[Task](1)
        block <- blockDagStorage.lookupByIdUnsafe(0)
        dag   <- blockDagStorage.getRepresentation
        _ <- Validate.sequenceNumber[Task](block.copy(seqNum = 1), dag) shouldBeF Left(
              InvalidSequenceNumber
            )
        _      <- Validate.sequenceNumber[Task](block, dag) shouldBeF Right(Valid)
        result = log.warns.size should be(1)
      } yield result
  }

  it should "return false for non-sequential numbering" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        _     <- createChain[Task](2)
        block <- blockDagStorage.lookupByIdUnsafe(1)
        dag   <- blockDagStorage.getRepresentation
        _ <- Validate.sequenceNumber[Task](block.copy(seqNum = 1), dag) shouldBeF Left(
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
        _ <- createChainWithRoundRobinValidators[Task](n, validatorCount)
        _ <- (0 until n).toList.forallM[Task](
              i =>
                for {
                  block <- blockDagStorage.lookupByIdUnsafe(i)
                  dag   <- blockDagStorage.getRepresentation
                  result <- Validate.sequenceNumber[Task](
                             block,
                             dag
                           )
                } yield result == Right(Valid)
            ) shouldBeF true
        result = log.warns should be(Nil)
      } yield result
  }

  "Repeat deploy validation" should "return valid for empty blocks" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        _      <- createChain[Task](2)
        block  <- blockDagStorage.lookupByIdUnsafe(0)
        block2 <- blockDagStorage.lookupByIdUnsafe(1)
        dag    <- blockDagStorage.getRepresentation
        _      <- Validate.repeatDeploy[Task](block, dag, 50) shouldBeF Right(Valid)
        _      <- Validate.repeatDeploy[Task](block2, dag, 50) shouldBeF Right(Valid)
      } yield ()
  }

  it should "not accept blocks with a repeated deploy" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        deploy  <- ConstructDeploy.basicProcessedDeploy[Task](0)
        genesis <- createGenesis[Task](deploys = Seq(deploy))
        block1 <- createBlock[Task](
                   Seq(genesis.blockHash),
                   genesis,
                   deploys = Seq(deploy)
                 )
        dag <- blockDagStorage.getRepresentation
        _   <- Validate.repeatDeploy[Task](block1, dag, 50) shouldBeF Left(InvalidRepeatDeploy)
      } yield ()
  }

  "Sender validation" should "return true for genesis and blocks from bonded validators and false otherwise" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val validator = generateValidator("Validator")
      val impostor  = generateValidator("Impostor")
      for {
        _            <- createChain[Task](3, List(Bond(validator, 1)))
        genesis      <- blockDagStorage.lookupByIdUnsafe(0)
        validBlock   <- blockDagStorage.lookupByIdUnsafe(1).map(_.copy(sender = validator))
        invalidBlock <- blockDagStorage.lookupByIdUnsafe(2).map(_.copy(sender = impostor))
        dag          <- blockDagStorage.getRepresentation
        _            <- Validate.blockSenderHasWeight[Task](genesis, genesis, dag) shouldBeF true
        _            <- Validate.blockSenderHasWeight[Task](validBlock, genesis, dag) shouldBeF true
        result       <- Validate.blockSenderHasWeight[Task](invalidBlock, genesis, dag) shouldBeF false
      } yield result
  }

  val genesisContext = GenesisBuilder.buildGenesis()
  val genesis        = genesisContext.genesisBlock

  "Parent validation" should "return true for proper justifications and false otherwise" ignore withGenesis(
    genesisContext
  ) { implicit blockStore => implicit blockDagStorage => runtimeManager =>
    val validotors          = GenesisBuilder.defaultValidatorPks
    val v0 +: v1 +: v2 +: _ = validotors
    val bonds               = GenesisBuilder.createBonds(validotors)

    def createValidatorBlock[F[_]: Monad: Time: BlockStore: IndexedBlockDagStorage](
        parents: Seq[BlockMessage],
        genesis: BlockMessage,
        justifications: Seq[BlockMessage],
        validator: PublicKey
    ): F[BlockMessage] =
      for {
        current <- Time[F].currentMillis
        deploy  <- ConstructDeploy.basicProcessedDeploy[F](current.toInt)
        block <- createBlock[F](
                  parents.map(_.blockHash),
                  genesis,
                  creator = ByteString.copyFrom(validator.bytes),
                  bonds = bonds.map { case (k, v) => Bond(ByteString.copyFrom(k.bytes), v) }.toSeq,
                  deploys = Seq(deploy),
                  justifications = latestMessages(justifications)
                )
      } yield block

    def latestMessages(messages: Seq[BlockMessage]): Map[Validator, BlockHash] =
      messages.map(b => b.sender -> b.blockHash).toMap

    val b0 = genesis

    for {
      _  <- IndexedBlockDagStorage[Task].insertIndexed(genesis, genesis, invalid = false)
      b1 <- createValidatorBlock[Task](Seq(b0), b0, Seq.empty, v0)
      b2 <- createValidatorBlock[Task](Seq(b0), b0, Seq.empty, v1)
      b3 <- createValidatorBlock[Task](Seq(b0), b0, Seq.empty, v2)
      b4 <- createValidatorBlock[Task](Seq(b1), b0, Seq(b1), v0)
      b5 <- createValidatorBlock[Task](Seq(b3, b2, b1), b0, Seq(b1, b2, b3), v1)
      b6 <- createValidatorBlock[Task](Seq(b5, b4), b0, Seq(b1, b4, b5), v0)
      b7 <- createValidatorBlock[Task](Seq(b4), b0, Seq(b1, b4, b5), v1) //not highest score parent
      b8 <- createValidatorBlock[Task](Seq(b1, b2, b3), b0, Seq(b1, b2, b3), v2) //parents wrong order
      b9 <- createValidatorBlock[Task](Seq(b6), b0, Seq.empty, v0) //empty justification

      _   <- step[Task](runtimeManager)(b1, b0)
      _   <- step[Task](runtimeManager)(b2, b0)
      _   <- step[Task](runtimeManager)(b3, b0)
      _   <- step[Task](runtimeManager)(b4, b0)
      _   <- step[Task](runtimeManager)(b5, b0)
      _   <- step[Task](runtimeManager)(b6, b0)
      _   <- step[Task](runtimeManager)(b7, b0)
      _   <- step[Task](runtimeManager)(b8, b0)
      _   <- step[Task](runtimeManager)(b9, b0)
      dag <- blockDagStorage.getRepresentation

      // Valid
      _ <- Validate.parents[Task](b0, b0, dag)
      _ <- Validate.parents[Task](b1, b0, dag)
      _ <- Validate.parents[Task](b2, b0, dag)
      _ <- Validate.parents[Task](b3, b0, dag)
      _ <- Validate.parents[Task](b4, b0, dag)
      _ <- Validate.parents[Task](b5, b0, dag)
      _ <- Validate.parents[Task](b6, b0, dag)
      _ = log.warns shouldBe empty

      // Not valid
      _ <- Validate.parents[Task](b7, b0, dag)
      _ <- Validate.parents[Task](b8, b0, dag)
      _ <- Validate.parents[Task](b9, b0, dag)

      result = log.warns.forall(
        _.matches(
          ".* block parents .* did not match estimate .* based on justification .*"
        )
      ) should be(
        true
      )
      _ = log.warns.size should be(3)
    } yield result
  }

  // Creates a block with an invalid block number and sequence number
  "Block summary validation" should "short circuit after first invalidity" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      for {
        _     <- createChain[Task](2)
        block <- blockDagStorage.lookupByIdUnsafe(1)
        dag   <- blockDagStorage.getRepresentation

        (sk, pk)         = Secp256k1.newKeyPair
        sender           = ByteString.copyFrom(pk.bytes)
        latestMessageOpt <- dag.latestMessage(sender)
        seqNum           = latestMessageOpt.fold(0)(_.seqNum) + 1
        signedBlock = ValidatorIdentity(sk).signBlock(
          block.withBlockNumber(17).copy(seqNum = 1)
        )
        _ <- Validate.blockSummary[Task](
              signedBlock,
              getRandomBlock(hashF = (ProtoUtil.hashUnsignedBlock _).some),
              dag,
              "root",
              Int.MaxValue
            ) shouldBeF Left(InvalidBlockNumber)
        result = log.warns.size should be(1)
      } yield result
  }

  "Justification follow validation" should "return valid for proper justifications and failed otherwise" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v1     = generateValidator("Validator One")
      val v2     = generateValidator("Validator Two")
      val v1Bond = Bond(v1, 2)
      val v2Bond = Bond(v2, 3)
      val bonds  = Seq(v1Bond, v2Bond)

      for {
        genesis <- createGenesis[Task](bonds = bonds)
        b2 <- createBlock[Task](
               Seq(genesis.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
             )
        b3 <- createBlock[Task](
               Seq(genesis.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash)
             )
        b4 <- createBlock[Task](
               Seq(b2.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash)
             )
        b5 <- createBlock[Task](
               Seq(b2.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash)
             )
        b6 <- createBlock[Task](
               Seq(b4.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
             )
        b7 <- createBlock[Task](
               Seq(b4.blockHash),
               genesis,
               v1,
               Seq(),
               HashMap(v1 -> b5.blockHash, v2 -> b4.blockHash)
             )
        b8 <- createBlock[Task](
               Seq(b7.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> b7.blockHash, v2 -> b4.blockHash)
             )
        _ <- (1 to 6).toList.forallM[Task](
              i =>
                for {
                  block <- blockDagStorage.lookupByIdUnsafe(i)
                  dag   <- blockDagStorage.getRepresentation
                  result <- Validate.justificationFollows[Task](
                             block,
                             genesis,
                             dag
                           )
                } yield result == Right(Valid)
            ) shouldBeF true
        blockId7 <- blockDagStorage.lookupByIdUnsafe(7)
        dag      <- blockDagStorage.getRepresentation
        _ <- Validate.justificationFollows[Task](
              blockId7,
              genesis,
              dag
            ) shouldBeF Left(InvalidFollows)
        _      = log.warns.size shouldBe 1
        result = log.warns.forall(_.contains("do not match the bonded validators")) shouldBe true
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
        b1 <- createValidatorBlock[Task](Seq(b0), b0, Seq(b0, b0), v0, bonds)
        b2 <- createValidatorBlock[Task](Seq(b1), b0, Seq(b1, b0), v0, bonds)
        b3 <- createValidatorBlock[Task](Seq(b0), b0, Seq(b2, b0), v1, bonds)
        b4 <- createValidatorBlock[Task](Seq(b3), b0, Seq(b2, b3), v1, bonds)
        _ <- (0 to 4).toList.forallM[Task](
              i =>
                for {
                  block <- blockDagStorage.lookupByIdUnsafe(i)
                  dag   <- blockDagStorage.getRepresentation
                  result <- Validate.justificationRegressions[Task](
                             block,
                             dag
                           )
                } yield result == Right(Valid)
            ) shouldBeF true
        // The justification block for validator 0 should point to b2 or above.
        justificationsWithRegression = Seq(
          Justification(v0, b1.blockHash),
          Justification(v1, b4.blockHash)
        )
        blockWithJustificationRegression = getRandomBlock(
          setValidator = v1.some,
          setJustifications = justificationsWithRegression.some,
          hashF = (ProtoUtil.hashUnsignedBlock _).some
        )
        dag <- blockDagStorage.getRepresentation
        _ <- Validate.justificationRegressions[Task](
              blockWithJustificationRegression,
              dag
            ) shouldBeF Left(JustificationRegression)
        result = log.warns.size shouldBe 1
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
        b1 <- createValidatorBlock[Task](Seq(b0), b0, Seq(b0, b0), v0, bonds, 1)
        b2 <- createValidatorBlock[Task](Seq(b0), b0, Seq(b1, b0), v1, bonds, 1)
        b3 <- createValidatorBlock[Task](Seq(b0), b0, Seq(b1, b2), v0, bonds, 2)
        b4 <- createValidatorBlock[Task](Seq(b0), b0, Seq(b3, b2), v1, bonds, 2)
        b5 <- createValidatorBlock[Task](Seq(b0), b0, Seq(b3, b4), v0, bonds, 1, invalid = true)

        justificationsWithInvalidBlock = Seq(
          Justification(v0, b5.blockHash),
          Justification(v1, b4.blockHash)
        )
        blockWithInvalidJustification = getRandomBlock(
          setValidator = v1.some,
          setJustifications = justificationsWithInvalidBlock.some
        )
        dag <- blockDagStorage.getRepresentation
        _ <- Validate.justificationRegressions[Task](
              blockWithInvalidJustification,
              dag
            ) shouldBeF Right(Valid)
      } yield ()
  }

  "Bonds cache validation" should "succeed on a valid block and fail on modified bonds" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val genesis = GenesisBuilder.createGenesis()

      val storageDirectory = Files.createTempDirectory(s"hash-set-casper-test-genesis-")

      for {
        kvm                  <- mkTestRNodeStoreManager[Task](storageDirectory)
        store                <- kvm.rSpaceStores
        spaces               <- Runtime.setupRSpace[Task](store)
        (rspace, replay, hr) = spaces
        runtime              <- Runtime.createWithEmptyCost((rspace, replay), Seq.empty)
        runtimeManager       <- RuntimeManager.fromRuntime[Task](runtime)
        dag                  <- blockDagStorage.getRepresentation
        _                    <- InterpreterUtil.validateBlockCheckpoint[Task](genesis, dag, runtimeManager)
        _                    <- Validate.bondsCache[Task](genesis, runtimeManager) shouldBeF Right(Valid)
        modifiedBonds        = Seq.empty[Bond]
        modifiedPostState    = genesis.body.state.copy(bonds = modifiedBonds.toList)
        modifiedBody         = genesis.body.copy(state = modifiedPostState)
        modifiedGenesis      = genesis.copy(body = modifiedBody)
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
        dag              <- blockDagStorage.getRepresentation
        sender           = ByteString.copyFrom(pk.bytes)
        latestMessageOpt <- dag.latestMessage(sender)
        seqNum           = latestMessageOpt.fold(0)(_.seqNum) + 1
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
        dag              <- blockDagStorage.getRepresentation
        latestMessageOpt <- dag.latestMessage(sender)
        seqNum           = latestMessageOpt.fold(0)(_.seqNum) + 1
        genesis = ValidatorIdentity(sk)
          .signBlock(context.genesisBlock.copy(seqNum = seqNum))
        _ <- Validate.blockHash[Task](genesis) shouldBeF Right(Valid)
        result <- Validate.blockHash[Task](
                   genesis.copy(blockHash = ByteString.copyFromUtf8("123"))
                 ) shouldBeF Left(InvalidBlockHash)
      } yield result
  }

  "Block version validation" should "work" in withStorage { _ => implicit blockDagStorage =>
    val context  = buildGenesis()
    val (sk, pk) = context.validatorKeyPairs.head
    val sender   = ByteString.copyFrom(pk.bytes)
    for {
      dag              <- blockDagStorage.getRepresentation
      latestMessageOpt <- dag.latestMessage(sender)
      seqNum           = latestMessageOpt.fold(0)(_.seqNum) + 1
      genesis = ValidatorIdentity(sk).signBlock(
        context.genesisBlock.copy(seqNum = seqNum)
      )
      _      <- Validate.version[Task](genesis, -1) shouldBeF false
      result <- Validate.version[Task](genesis, 1) shouldBeF true
    } yield result
  }

}
