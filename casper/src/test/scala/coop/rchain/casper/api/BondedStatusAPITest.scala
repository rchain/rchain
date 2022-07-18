package coop.rchain.casper.api

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag._
import coop.rchain.casper.ValidatorIdentity
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol.{BlockMessage, BlockMessageProto}
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.GenesisBuilder._
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.metrics.Span
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.shared.Log
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.mockito.cats.IdiomaticMockitoCats
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap

class BondedStatusAPITest
    extends AnyFlatSpec
    with Matchers
    with EitherValues
    with BlockGenerator
    with BlockDagStorageFixture
    with BlockApiFixture
    with IdiomaticMockito
    with IdiomaticMockitoCats
    with ArgumentMatchersSugar {
  // 4 nodes with 3 validators bonded
  private val keys  = randomValidatorKeyPairs.take(3).toList :+ ConstructDeploy.defaultKeyPair
  private val bonds = createBonds(keys.map(_._2).take(3))
  private val initialComputeBondsResult = keys
    .zip(bonds)
    .map { case ((_, pubKey), (_, bond)) => pubKey.bytes.toByteString -> bond }
    .toMap
  private val gB =
    makeBlock(bonds = initialComputeBondsResult, validatorId = ValidatorIdentity(keys.head._1))

  "bondStatus" should "return true for bonded validator" in effectTest {
    implicit val (c, log, bds, bs, rm, sp) = createMocks[Task]

    val v1 = ValidatorIdentity(keys.head._1)
    val v2 = ValidatorIdentity(keys(1)._1)
    val v3 = ValidatorIdentity(keys(2)._1)

    bondedStatus(v1, v1.publicKey, gB) shouldBeF true
    bondedStatus(v2, v2.publicKey, gB) shouldBeF true
    bondedStatus(v3, v3.publicKey, gB) shouldBeF true
  }

  "bondStatus" should "return false for not bonded validators" in effectTest {
    implicit val (c, log, bds, bs, rm, sp) = createMocks[Task]
    val (_, publicKey)                     = Secp256k1.newKeyPair
    val genesisValidator                   = ValidatorIdentity(keys.head._1)

    bondedStatus(genesisValidator, publicKey, gB) shouldBeF false
  }

  "bondStatus" should "return true for newly bonded validator" in effectTest {
    implicit val (c, log, bds, bs, _, sp) = createMocks[Task]

    // Overriding mock for RuntimeManager, as it differ from the standard one
    val stake                             = 1000L
    val newComputeBondsResult             = initialComputeBondsResult + (keys.last._2.bytes.toByteString -> stake)
    implicit val rm: RuntimeManager[Task] = mock[RuntimeManager[Task]]
    rm.computeBonds(*) returns initialComputeBondsResult.pure andThen newComputeBondsResult.pure

    val genesisValidator = ValidatorIdentity(keys.head._1)
    val newValidator     = ValidatorIdentity(keys.last._1)

    for {
      _  <- BondingUtil.bondingDeploy[Task](stake, newValidator.privateKey, shardId = gB.shardId)
      _  <- bondedStatus(genesisValidator, newValidator.publicKey, gB) shouldBeF false
      b1 = makeBlock(List(gB.blockHash), newComputeBondsResult, newValidator)

      // b1 is now finalized, hence n4 is now bonded
      _ <- bondedStatus(genesisValidator, newValidator.publicKey, b1) shouldBeF true
    } yield ()
  }

  private def makeDefaultBlock =
    BlockMessage
      .from(
        BlockMessageProto(
          shardId = "root",
          postStateHash = "abc".unsafeHexToByteString,
          sigAlgorithm = Secp256k1.name
        )
      )
      .right
      .get

  def makeBlock(
      justifications: List[BlockHash] = List(),
      bonds: Map[Validator, Long],
      validatorId: ValidatorIdentity
  ): BlockMessage = {
    val (privateKey, pubKey) = (validatorId.privateKey, validatorId.publicKey)
    val block =
      makeDefaultBlock.copy(
        sender = pubKey.bytes.toByteString,
        justifications = justifications,
        bonds = bonds
      )
    ValidatorIdentity(privateKey).signBlock(block)
  }

  private def createMocks[F[_]: Concurrent: Sync] = {
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
      Map(Set(gB.blockHash) -> (gB.blockHash.toBlake2b256Hash, Set()))
    )

    val bs = mock[BlockStore[F]]
    bs.get(*) returnsF Vector(gB.some)

    val rm = mock[RuntimeManager[F]]
    rm.computeBonds(*) returnsF initialComputeBondsResult

    (c, log, bds, bs, rm, sp)
  }

  private def toMessage(m: BlockMessage) = Message[BlockHash, Validator](
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
}
