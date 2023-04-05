package coop.rchain.casper.api

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{Async, IO, Sync}
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
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.metrics.Span
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.FringeData
import coop.rchain.models.Validator.Validator
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.models.syntax._
import coop.rchain.shared.{Log, Time}
import coop.rchain.shared.scalatestcontrib._
import org.mockito.cats.IdiomaticMockitoCats
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap

// TODO enable when CE is migrated to 3 (cats.effect.testing.scalatest is not available for CE2)
//class BondedStatusAPITest
//    extends AsyncFlatSpec
//    with AsyncIOSpec
//    with Matchers
//    with EitherValues
//    with BlockGenerator
//    with BlockDagStorageFixture
//    with BlockApiFixture
//    with IdiomaticMockito
//    with IdiomaticMockitoCats
//    with ArgumentMatchersSugar {
//  // 4 nodes with 3 validators bonded
//  private val keys = randomValidatorKeyPairs.take(3).toList
//  private val initialComputeBondsResult = keys
//    .zip(createBonds(keys.map(_._2)))
//    .map { case ((_, pubKey), (_, bond)) => pubKey.bytes.toByteString -> bond }
//    .toMap
//  private val gB = getRandomBlock(
//    setBonds = initialComputeBondsResult.some,
//    setValidator = toValidatorOpt(keys.head._1)
//  )
//  import coop.rchain.shared.RChainScheduler._
//
//  "bondStatus" should "return true for bonded validator" in {
//    implicit val (c, log, bds, bs, rm, sp) = createMocks[IO]
//
//    for {
//      v1 <- IO.delay(ValidatorIdentity(keys.head._1))
//      v2 = ValidatorIdentity(keys(1)._1)
//      v3 = ValidatorIdentity(keys(2)._1)
//
//      _ <- bondedStatus(v1, v1.publicKey, gB) shouldBeF true
//      _ <- bondedStatus(v2, v2.publicKey, gB) shouldBeF true
//      _ <- bondedStatus(v3, v3.publicKey, gB) shouldBeF true
//    } yield {
//      bs.get(Seq(gB.blockHash)) wasCalled 3.times
//      verifyNoMoreInteractions(bs)
//      bds.getRepresentation wasCalled 3.times
//      rm.computeBonds(gB.postStateHash) wasCalled 3.times
//    }
//  }
//
//  "bondStatus" should "return false for not bonded validators" in {
//    implicit val (c, log, bds, bs, rm, sp) = createMocks[IO]
//    val genesisValidator                   = ValidatorIdentity(keys.head._1)
//    for {
//      _ <- bondedStatus(genesisValidator, createValidator.publicKey, gB) shouldBeF false
//    } yield {
//      bs.get(Seq(gB.blockHash)) wasCalled once
//      verifyNoMoreInteractions(bs)
//      bds.getRepresentation wasCalled once
//      rm.computeBonds(gB.postStateHash) wasCalled once
//    }
//  }
//
//  "bondStatus" should "return true for newly bonded validator" in {
//    implicit val (c, log, bds, bs, _, sp) = createMocks[IO]
//
//    val genesisValidator = ValidatorIdentity(keys.head._1)
//    val newValidator     = createValidator
//
//    // Overriding mock for RuntimeManager, as it differ from the standard one
//    val stake                           = 1000L
//    val newComputeBondsResult           = initialComputeBondsResult + (newValidator.publicKey.bytes.toByteString -> stake)
//    implicit val rm: RuntimeManager[IO] = mock[RuntimeManager[IO]]
//    rm.computeBonds(*) returns initialComputeBondsResult.pure andThen newComputeBondsResult.pure
//
//    for {
//      _ <- BondingUtil.bondingDeploy[IO](stake, newValidator.privateKey, shardId = gB.shardId)
//      _ <- bondedStatus(genesisValidator, newValidator.publicKey, gB) shouldBeF false
//      b1 = getRandomBlock(
//        setJustifications = Seq(gB.blockHash).some,
//        setBonds = newComputeBondsResult.some,
//        setValidator = toValidatorOpt(newValidator.privateKey)
//      )
//
//      // b1 is now finalized, hence n4 is now bonded
//      _ <- bondedStatus(genesisValidator, newValidator.publicKey, b1) shouldBeF true
//    } yield {
//      bs.get(Seq(gB.blockHash)) wasCalled twice
//      verifyNoMoreInteractions(bs)
//      bds.getRepresentation wasCalled twice
//      rm.computeBonds(gB.postStateHash) wasCalled once
//      rm.computeBonds(b1.postStateHash) wasCalled once
//    }
//  }
//
//  private def createMocks[F[_]: Async: Sync]
//      : (Concurrent[F], Log[F], BlockDagStorage[F], BlockStore[F], RuntimeManager[F], Span[F]) = {
//    val c  = Concurrent[F]
//    val sp = mock[Span[F]]
//
//    val log = mock[Log[F]]
//    log.warn(*) returns ().pure
//
//    val msg = toMessage(gB)
//    val bds = mock[BlockDagStorage[F]]
//    bds.getRepresentation returnsF DagRepresentation(
//      Set(gB.blockHash),
//      Map(gB.blockHash -> Set()),
//      SortedMap(0L     -> Set(gB.blockHash)),
//      new DagMessageState(Set(msg), Map(msg.id -> msg)),
//      Map(
//        Set(gB.blockHash) -> FringeData(
//          FringeData.fringeHash(Set.empty),
//          Set.empty,
//          Set.empty,
//          gB.blockHash.toBlake2b256Hash,
//          Set.empty,
//          Set.empty,
//          Set.empty
//        )
//      )
//    )
//
//    val bs = mock[BlockStore[F]]
//    bs.get(Seq(gB.blockHash)) returnsF Vector(gB.some)
//
//    val rm = mock[RuntimeManager[F]]
//    rm.computeBonds(*) returnsF initialComputeBondsResult
//
//    (c, log, bds, bs, rm, sp)
//  }
//
//  private def toValidatorOpt(pk: PrivateKey): Option[Validator] = pk.bytes.toByteString.some
//
//  private def toMessage(m: BlockMessage): Message[BlockHash, Validator] =
//    Message[BlockHash, Validator](
//      m.blockHash,
//      m.blockNumber,
//      m.sender,
//      m.seqNum,
//      m.bonds,
//      m.justifications.toSet,
//      Set(m.blockHash),
//      Set(m.blockHash)
//    )
//
//  private def bondedStatus[F[_]: Async: BlockDagStorage: BlockStore: Log: RuntimeManager: Span](
//      validatorIdOpt: ValidatorIdentity,
//      publicKey: PublicKey,
//      block: BlockMessage
//  ): F[Boolean] =
//    for {
//      blockApi <- createBlockApi("root", 50, validatorIdOpt.some)
//      res      <- blockApi.bondStatus(ByteString.copyFrom(publicKey.bytes), block.some).map(_.value)
//    } yield res
//
//  private def createValidator: ValidatorIdentity = {
//    val (privateKey, _) = Secp256k1.newKeyPair
//    ValidatorIdentity(privateKey)
//  }
//}
