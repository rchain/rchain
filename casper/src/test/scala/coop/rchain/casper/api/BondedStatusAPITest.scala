package coop.rchain.casper.api

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag._
import coop.rchain.casper.ValidatorIdentity
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.util.GenesisBuilder._
import coop.rchain.crypto.PublicKey
import coop.rchain.models.syntax._
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.metrics.Span
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.FringeData
import coop.rchain.models.Validator.Validator
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.models.syntax._
import coop.rchain.shared.Log
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.testing.scalatest.MonixTaskTest
import org.mockito.cats.IdiomaticMockitoCats
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap

class BondedStatusAPITest
    extends AsyncFlatSpec
    with MonixTaskTest
    with Matchers
    with EitherValues
    with BlockGenerator
    with BlockDagStorageFixture
    with BlockApiFixture
    with IdiomaticMockito
    with IdiomaticMockitoCats
    with ArgumentMatchersSugar {
  // 4 nodes with 3 validators bonded
  private val keys = randomValidatorKeyPairs.take(3).toList
  val bonds        = createBonds(keys.map(_._2).take(3))
  private val initialComputeBondsResult = keys
    .zip(createBonds(keys.map(_._2)))
    .map { case ((_, pubKey), (_, bond)) => pubKey.bytes.toByteString -> bond }
    .toMap
  private val gB = getRandomBlock(
    setBonds = initialComputeBondsResult.some,
    setValidator = toValidatorOpt(keys.head._1)
  )
  val genesisContext = buildGenesis(buildGenesisParameters(keys)(bonds))

  "bondStatus" should "return true for bonded validator" in {
    implicit val (c, log, bds, bs, rm, sp) = createMocks[Task]

    for {
      v1 <- Sync[Task].delay(ValidatorIdentity(keys.head._1))
      v2 = ValidatorIdentity(keys(1)._1)
      v3 = ValidatorIdentity(keys(2)._1)

      _ <- bondedStatus(v1, v1.publicKey, gB) shouldBeF true
      _ <- bondedStatus(v2, v2.publicKey, gB) shouldBeF true
      _ <- bondedStatus(v3, v3.publicKey, gB) shouldBeF true
    } yield {
      bs.get(Seq(gB.blockHash)) wasCalled 3.times
      verifyNoMoreInteractions(bs)
      bds.getRepresentation wasCalled 3.times
      rm.computeBonds(gB.postStateHash) wasCalled 3.times
    }
  }

  "bondStatus" should "return false for not bonded validators" in {
    implicit val (c, log, bds, bs, rm, sp) = createMocks[Task]
    val genesisValidator                   = ValidatorIdentity(keys.head._1)
    for {
      _ <- bondedStatus(genesisValidator, createValidator.publicKey, gB) shouldBeF false
    } yield {
      bs.get(Seq(gB.blockHash)) wasCalled once
      verifyNoMoreInteractions(bs)
      bds.getRepresentation wasCalled once
      rm.computeBonds(gB.postStateHash) wasCalled once
    }
  }

  "bondStatus" should "return true for newly bonded validator" ignore effectTest {
    TestNode.networkEff(genesisContext, networkSize = 4).use {
      case nodes @ n1 +: _ +: _ +: n4 +: _ =>
        implicit val (c, log, bds, bs, rm, sp) = createMocks[Task]
        val amount                             = 1000L
        for {
          addBondingWhiteList <- BondingUtil.addValidatorInBondingWhiteList[Task](
                                  amount,
                                  defaultPosVaultPrivateKey,
                                  n4.validatorIdOpt.get.publicKey.bytes.toHexString,
                                  genesisContext.genesisBlock.shardId
                                )
          bondDeploy <- BondingUtil.bondingDeploy[Task](
                         amount,
                         n4.validatorIdOpt.get.privateKey,
                         shardId = genesisContext.genesisBlock.shardId
                       )

          _  <- bondedStatus(n1.validatorIdOpt.get, n4.validatorIdOpt.get.publicKey, gB) shouldBeF false
          b1 <- n1.propagateBlock(addBondingWhiteList)(nodes: _*)
          b2 <- n1.propagateBlock(bondDeploy)(nodes: _*)
          _  <- bondedStatus(n1.validatorIdOpt.get, n4.validatorIdOpt.get.publicKey, b2) shouldBeF true
        } yield assert(true)
    }
  }

  private def createMocks[F[_]: Concurrent: Sync]
      : (Concurrent[F], Log[F], BlockDagStorage[F], BlockStore[F], RuntimeManager[F], Span[F]) = {
    val c  = Concurrent[F]
    val sp = mock[Span[F]]

    val log = mock[Log[F]]
    log.warn(*) returns ().pure

    val msg = toMessage(gB)
    val bds = mock[BlockDagStorage[F]]
    bds.getRepresentation returnsF DagRepresentation(
      Set(gB.blockHash),
      Map(gB.blockHash -> Set()),
      SortedMap(0L     -> Set(gB.blockHash)),
      new DagMessageState(Set(msg), Map(msg.id -> msg)),
      Map(
        Set(gB.blockHash) -> FringeData(
          FringeData.fringeHash(Set.empty),
          Set.empty,
          Set.empty,
          gB.blockHash.toBlake2b256Hash,
          Set.empty,
          Set.empty,
          Set.empty
        )
      )
    )

    val bs = mock[BlockStore[F]]
    bs.get(Seq(gB.blockHash)) returnsF Vector(gB.some)

    val rm = mock[RuntimeManager[F]]
    rm.computeBonds(*) returnsF initialComputeBondsResult

    (c, log, bds, bs, rm, sp)
  }

  private def toValidatorOpt(pk: PrivateKey): Option[Validator] = pk.bytes.toByteString.some

  private def toMessage(m: BlockMessage): Message[BlockHash, Validator] =
    Message[BlockHash, Validator](
      m.blockHash,
      m.blockNumber,
      m.sender,
      m.seqNum,
      m.bonds,
      m.justifications.toSet,
      Set(m.blockHash),
      Set(m.blockHash)
    )

  private def bondedStatus[F[_]: Concurrent: BlockDagStorage: BlockStore: Log: RuntimeManager: Span](
      validatorIdOpt: ValidatorIdentity,
      publicKey: PublicKey,
      block: BlockMessage
  ): F[Boolean] =
    for {
      blockApi <- createBlockApi("root", 50, validatorIdOpt.some)
      res      <- blockApi.bondStatus(ByteString.copyFrom(publicKey.bytes), block.some).map(_.value)
    } yield res

  private def createValidator: ValidatorIdentity = {
    val (privateKey, _) = Secp256k1.newKeyPair
    ValidatorIdentity(privateKey)
  }
}
