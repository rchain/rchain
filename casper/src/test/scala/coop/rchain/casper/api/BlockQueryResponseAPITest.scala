package coop.rchain.casper.api

import cats.effect.{IO, Sync}
import cats.effect.concurrent.Ref
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag._
import coop.rchain.casper._
import coop.rchain.casper.helper.{BlockApiFixture, BlockDagStorageFixture}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.metrics.NoopSpan
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.block.StateHash.StateHash
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockMetadata, FringeData}
import coop.rchain.shared.{Log, Time}
import org.mockito.cats.IdiomaticMockitoCats
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito, Mockito, MockitoSugar}
import org.scalatest._
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import coop.rchain.shared.RChainScheduler._

import scala.collection.immutable.SortedMap

// TODO enable when CE is migrated to 3 (cats.effect.testing.scalatest is not available for CE2)
//class BlockQueryResponseAPITest
//    extends AsyncFlatSpec
//    with AsyncIOSpec
//    with Matchers
//    with EitherValues
//    with BlockDagStorageFixture
//    with BlockApiFixture
//    with IdiomaticMockito
//    with IdiomaticMockitoCats
//    with ArgumentMatchersSugar {
//  implicit val timeEff: Time[IO]                  = Time.fromTimer[IO]
//  implicit val spanEff: NoopSpan[IO]              = NoopSpan[IO]()
//  implicit val log: Log[IO]                       = mock[Log[IO]]
//  implicit val runtimeManager: RuntimeManager[IO] = mock[RuntimeManager[IO]]
//
//  private val tooShortQuery    = "12345"
//  private val badTestHashQuery = "1234acd"
//  private val invalidHexQuery  = "No such a hash"
//  private val unknownDeploy    = ByteString.copyFromUtf8("asdfQwertyUiopxyzcbv")
//
//  private val genesisBlock: BlockMessage = getRandomBlock(setJustifications = Seq().some)
//
//  private val deployCount = 10
//  private val randomDeploys =
//    (0 until deployCount).toList
//      .traverse(i => ConstructDeploy.basicProcessedDeploy[IO](i))
//      .unsafeRunSync
//
//  private val senderString: String =
//    "3456789101112131415161718192345678910111213141516171819261718192113456789101112131415161718192345678910111213141516171819261718192"
//  private val sender: ByteString = senderString.unsafeHexToByteString
//  private val bondsValidator     = (sender, 1L)
//
//  private val secondBlock: BlockMessage =
//    getRandomBlock(
//      setValidator = sender.some,
//      setDeploys = randomDeploys.some,
//      setJustifications = List(genesisBlock.blockHash).some,
//      setBonds = Map(bondsValidator).some
//    )
//
//  "getBlock" should "return successful block info response" in {
//    implicit val bs  = createBlockStore[IO]
//    implicit val bds = createBlockDagStorage[IO]
//
//    for {
//      _                  <- prepareDagStorage[IO]
//      blockApi           <- createBlockApi[IO]("", 1)
//      _                  = Mockito.clearInvocations(bs, bds)
//      hash               = secondBlock.blockHash.toHexString
//      blockQueryResponse <- blockApi.getBlock(hash)
//    } yield {
//      blockQueryResponse shouldBe 'right
//      val blockInfo = blockQueryResponse.value
//      blockInfo.deploys shouldBe randomDeploys.map(_.toDeployInfo)
//      blockInfo.blockInfo shouldBe BlockApi.getLightBlockInfo(secondBlock)
//
//      bs.get(Seq(secondBlock.blockHash)) wasCalled once
//      verifyNoMoreInteractions(bs)
//
//      bds.insert(*, *) wasNever called
//      bds.getRepresentation wasCalled twice
//      bds.lookupByDeployId(*) wasNever called
//    }
//  }
//
//  it should "return error when no block exists" in {
//    implicit val bs  = createBlockStore[IO]
//    implicit val bds = createBlockDagStorage[IO]
//
//    for {
//      blockApi           <- createBlockApi[IO]("", 1)
//      hash               = badTestHashQuery
//      blockQueryResponse <- blockApi.getBlock(hash)
//    } yield {
//      blockQueryResponse shouldBe 'left
//      blockQueryResponse.left.value shouldBe s"Error: Failure to find block with hash: $badTestHashQuery"
//
//      bs.get(Seq(badTestHashQuery.unsafeHexToByteString)) wasCalled once
//      verifyNoMoreInteractions(bs)
//
//      bds.insert(*, *) wasNever called
//      bds.getRepresentation wasCalled once
//      bds.lookupByDeployId(*) wasNever called
//    }
//  }
//
//  it should "return error when hash is invalid hex string" in {
//    implicit val bs  = createBlockStore[IO]
//    implicit val bds = createBlockDagStorage[IO]
//
//    for {
//      blockApi           <- createBlockApi[IO]("", 1)
//      hash               = invalidHexQuery
//      blockQueryResponse <- blockApi.getBlock(hash)
//    } yield {
//      blockQueryResponse shouldBe 'left
//      blockQueryResponse.left.value shouldBe s"Input hash value is not valid hex string: $invalidHexQuery"
//
//      verifyNoMoreInteractions(bs)
//
//      bds.insert(*, *) wasNever called
//      bds.getRepresentation wasNever called
//      bds.lookupByDeployId(*) wasNever called
//    }
//  }
//
//  it should "return error when hash is to short" in {
//    implicit val bs  = createBlockStore[IO]
//    implicit val bds = createBlockDagStorage[IO]
//
//    for {
//      blockApi           <- createBlockApi[IO]("", 1)
//      hash               = tooShortQuery
//      blockQueryResponse <- blockApi.getBlock(hash)
//    } yield {
//      blockQueryResponse shouldBe 'left
//      blockQueryResponse.left.value shouldBe s"Input hash value must be at least 6 characters: $tooShortQuery"
//
//      verifyNoMoreInteractions(bs)
//
//      bds.insert(*, *) wasNever called
//      bds.getRepresentation wasNever called
//      bds.lookupByDeployId(*) wasNever called
//    }
//  }
//
//  "findDeploy" should "return successful block info response when a block contains the deploy with given signature" in {
//    implicit val bs  = createBlockStore[IO]
//    implicit val bds = createBlockDagStorage[IO]
//
//    for {
//      _                  <- prepareDagStorage[IO]
//      blockApi           <- createBlockApi[IO]("", 1)
//      _                  = Mockito.clearInvocations(bs, bds)
//      deployId           = randomDeploys.head.deploy.sig
//      blockQueryResponse <- blockApi.findDeploy(deployId)
//    } yield {
//      blockQueryResponse shouldBe 'right
//      blockQueryResponse.value shouldBe BlockApi.getLightBlockInfo(secondBlock)
//
//      bs.get(Seq(secondBlock.blockHash)) wasCalled once
//      verifyNoMoreInteractions(bs)
//
//      bds.insert(*, *) wasNever called
//      bds.getRepresentation wasNever called
//      bds.lookupByDeployId(deployId) wasCalled once
//    }
//  }
//
//  it should "return an error when no block contains the deploy with the given signature" in {
//    implicit val bs  = createBlockStore[IO]
//    implicit val bds = createBlockDagStorage[IO]
//
//    for {
//      blockApi           <- createBlockApi[IO]("", 1)
//      blockQueryResponse <- blockApi.findDeploy(unknownDeploy)
//    } yield {
//      blockQueryResponse shouldBe 'left
//      blockQueryResponse.left.value shouldBe
//        s"Couldn't find block containing deploy with id: ${PrettyPrinter.buildStringNoLimit(unknownDeploy)}"
//
//      verifyNoMoreInteractions(bs)
//
//      bds.insert(*, *) wasNever called
//      bds.getRepresentation wasNever called
//      bds.lookupByDeployId(unknownDeploy) wasCalled once
//    }
//  }
//
//  private def createBlockStore[F[_]: Sync] = {
//    val bs = mock[BlockStore[F]]
//    bs.put(Seq((genesisBlock.blockHash, genesisBlock))) returns ().pure
//    bs.put(Seq((secondBlock.blockHash, secondBlock))) returns ().pure
//    bs.get(Seq(secondBlock.blockHash)) returnsF Seq(secondBlock.some)
//    bs.get(Seq(badTestHashQuery.unsafeHexToByteString)) returnsF Seq(None)
//    bs
//  }
//
//  private def createBlockDagStorage[F[_]: Sync]: BlockDagStorage[F] = {
//    val genesisHash: ByteString = RuntimeManager.emptyStateHashFixed
//
//    val state = Ref.unsafe[F, DagRepresentation](
//      DagRepresentation(
//        Set(),
//        Map(),
//        SortedMap(),
//        DagMessageState(),
//        Map(
//          Set(genesisHash) -> FringeData(
//            FringeData.fringeHash(Set.empty),
//            Set.empty,
//            Set.empty,
//            genesisHash.toBlake2b256Hash,
//            Set.empty,
//            Set.empty,
//            Set.empty
//          )
//        )
//      )
//    )
//
//    val bds = mock[BlockDagStorage[F]]
//
//    bds.insert(*, *) answers { (bmd: BlockMetadata, b: BlockMessage) =>
//      state.update { s =>
//        val newDagSet = s.dagSet + b.blockHash
//
//        val newChildMap = b.justifications.foldLeft(s.childMap) {
//          case (m, h) => m + (h -> (m.getOrElse(h, Set.empty) + b.blockHash))
//        } + (b.blockHash -> Set.empty[BlockHash])
//
//        val newHeightMap = s.heightMap + (b.blockNumber -> (s.heightMap
//          .getOrElse(b.blockNumber, Set.empty) + b.blockHash))
//
//        val seen = b.justifications
//          .flatMap(h => s.dagMessageState.msgMap(h).seen)
//          .toSet ++ b.justifications + b.blockHash
//
//        val newMsgMap = s.dagMessageState.msgMap + (b.blockHash -> toMessage(b, seen))
//
//        val newLatestMsgs = newMsgMap.foldLeft(Set.empty[Message[BlockHash, Validator]]) {
//          case (acc, (_, msg)) =>
//            acc + acc
//              .find(_.sender == msg.sender)
//              .map(m => if (msg.height > m.height) msg else m)
//              .getOrElse(msg)
//        }
//        val newDagMessageState = s.dagMessageState.copy(newLatestMsgs, newMsgMap)
//
//        s.copy(
//          dagSet = newDagSet,
//          childMap = newChildMap,
//          heightMap = newHeightMap,
//          dagMessageState = newDagMessageState
//        )
//      }
//    }
//
//    bds.getRepresentation returns state.get
//
//    bds.lookupByDeployId(randomDeploys.head.deploy.sig) returnsF secondBlock.blockHash.some
//    bds.lookupByDeployId(unknownDeploy) returnsF None
//
//    bds
//  }
//
//  // Default args only available for public method in Scala 2.12 (https://github.com/scala/bug/issues/12168)
//  def toMessage(
//      m: BlockMessage,
//      seen: Set[BlockHash] = Set.empty[BlockHash]
//  ): Message[BlockHash, Validator] =
//    Message[BlockHash, Validator](
//      m.blockHash,
//      m.blockNumber,
//      m.sender,
//      m.seqNum,
//      m.bonds,
//      m.justifications.toSet,
//      Set(),
//      seen
//    )
//
//  private def prepareDagStorage[F[_]: Sync: BlockDagStorage: BlockStore]: F[Unit] = {
//    import coop.rchain.blockstorage.syntax._
//    def insertToDag(b: BlockMessage, stateHash: StateHash): F[Unit] =
//      BlockDagStorage[F].insert(BlockMetadata.fromBlock(b).copy(fringeStateHash = stateHash), b)
//    for {
//      _ <- List(genesisBlock, secondBlock).traverse(BlockStore[F].put(_))
//      _ <- insertToDag(genesisBlock, genesisBlock.postStateHash)
//      _ <- insertToDag(secondBlock, RuntimeManager.emptyStateHashFixed)
//    } yield ()
//  }
//}
