package coop.rchain.casper.api

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.shared.Log
import monix.eval.Task
import org.mockito.cats.IdiomaticMockitoCats
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap

// See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
class BlocksResponseAPITest
    extends AnyFlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture
    with BlockApiFixture
    with IdiomaticMockito
    with IdiomaticMockitoCats
    with ArgumentMatchersSugar {

  implicit val log: Log[Task]       = new Log.NOPLog[Task]()
  implicit val noopSpan: Span[Task] = NoopSpan[Task]()

  val v1: Validator               = generateValidator("Validator One")
  val v2: Validator               = generateValidator("Validator Two")
  val v3: Validator               = generateValidator("Validator Three")
  val v1Bond: (Validator, Long)   = (v1, 25L)
  val v2Bond: (Validator, Long)   = (v2, 20L)
  val v3Bond: (Validator, Long)   = (v3, 15L)
  val bonds: Map[Validator, Long] = Map(v1Bond, v2Bond, v3Bond)
  val maxBlockLimit               = 50

  private def createDagWith8Blocks(
      implicit blockstore: BlockStore[Task],
      dagstore: BlockDagStorage[Task]
  ) =
    for {
      genesis <- createGenesis[Task](bonds = bonds)
      b2 <- createBlock[Task](
             v2,
             bonds,
             Seq(genesis.blockHash)
           )
      b3 <- createBlock[Task](
             v1,
             bonds,
             Seq(genesis.blockHash)
           )
      b4 <- createBlock[Task](
             v3,
             bonds,
             Seq(genesis.blockHash, b2.blockHash)
           )
      b5 <- createBlock[Task](
             v2,
             bonds,
             Seq(b3.blockHash, b2.blockHash, genesis.blockHash)
           )
      b6 <- createBlock[Task](
             v1,
             bonds,
             Seq(b3.blockHash, b2.blockHash, b4.blockHash)
           )
      b7 <- createBlock[Task](
             v3,
             bonds,
             Seq(b3.blockHash, b5.blockHash, b4.blockHash)
           )
      b8 <- createBlock[Task](
             v2,
             bonds,
             Seq(b6.blockHash, b5.blockHash, b4.blockHash)
           )
    } yield genesis

  "getBlocks" should "return all blocks" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = createMocks[Task]()

    for {
      genesis        <- createDagWith8Blocks(blockStore, blockDagStorage)
      blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit)
      blocksResponse <- blockApi.getBlocks(10)
    } yield blocksResponse.right.get.length should be(8)
  }

  it should "return until depth" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = createMocks[Task]()

    for {
      genesis        <- createDagWith8Blocks(blockStore, blockDagStorage)
      blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit)
      blocksResponse <- blockApi.getBlocks(2)
    } yield blocksResponse.right.get.length should be(3)
  }

  "getBlocksByHeights" should "return blocks between startBlockNumber and endBlockNumber" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = createMocks[Task]()

    for {
      genesis        <- createDagWith8Blocks(blockStore, blockDagStorage)
      blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit)
      blocksResponse <- blockApi.getBlocksByHeights(2, 5)
      blocks         = blocksResponse.right.get
      _              = blocks.length should be(5)
      _              = blocks.head.blockNumber should be(2)
      _              = blocks.last.blockNumber should be(4)
    } yield ()
  }

  private def createMocks[F[_]: Sync]() =
    (createBlockStoreMock[F], createBlockDagStorageMock[F], mock[RuntimeManager[F]])

  private def createBlockStoreMock[F[_]: Sync] = {
    val state = Ref.unsafe[F, List[BlockMessage]](List())
    val bs    = mock[BlockStore[F]]

    bs.put(*) answers { kvPairs: Seq[(BlockHash, BlockMessage)] =>
      state.update(s => kvPairs.foldLeft(s) { case (acc, item) => acc :+ item._2 })
    }
    bs.get(*) answers { keys: Seq[BlockHash] =>
      state.get.map(s => keys.map(h => s.find(_.blockHash == h)))
    }
    bs
  }

  private def createBlockDagStorageMock[F[_]: Sync] = {
    val genesisHash: ByteString =
      "9619d9a34bdaf56d5de8cfb7c2304d63cd9e469a0bfc5600fd2f5b9808e290f1".unsafeHexToByteString

    val state = Ref.unsafe[F, DagRepresentation](
      DagRepresentation(
        Set(),
        Map(),
        SortedMap(),
        DagMessageState(),
        Map(Set() -> (Blake2b256Hash.fromByteString(genesisHash), Set()))
      )
    )
    val bds = mock[BlockDagStorage[F]]

    bds.insert(*, invalid = false) answers { (b: BlockMessage) =>
      state.updateAndGet { s =>
        val newDagSet = s.dagSet + b.blockHash

        val newChildMap = b.justifications.foldLeft(s.childMap) {
          case (m, h) => m + (h -> (m.getOrElse(h, Set.empty) + b.blockHash))
        } + (b.blockHash -> Set.empty[BlockHash])

        val newHeightMap = s.heightMap + (b.blockNumber -> (s.heightMap
          .getOrElse(b.blockNumber, Set.empty) + b.blockHash))

        val seen = b.justifications
          .flatMap(h => s.dagMessageState.msgMap(h).seen)
          .toSet ++ b.justifications + b.blockHash

        val newMsgMap = s.dagMessageState.msgMap + (b.blockHash -> toMessage(b, seen))

        val newLatestMsgs = newMsgMap.foldLeft(Set.empty[Message[BlockHash, Validator]]) {
          case (acc, (_, msg)) =>
            acc + acc
              .find(_.sender == msg.sender)
              .map(m => if (msg.height > m.height) msg else m)
              .getOrElse(msg)
        }
        val newDagMessageState = s.dagMessageState.copy(newLatestMsgs, newMsgMap)

        s.copy(
          dagSet = newDagSet,
          childMap = newChildMap,
          heightMap = newHeightMap,
          dagMessageState = newDagMessageState
        )
      }
    }

    bds.getRepresentation returns state.get

    bds
  }

  // Default args only available for public method in Scala 2.12 (https://github.com/scala/bug/issues/12168)
  def toMessage(
      m: BlockMessage,
      seen: Set[BlockHash] = Set.empty[BlockHash]
  ): Message[BlockHash, Validator] =
    Message[BlockHash, Validator](
      m.blockHash,
      m.blockNumber,
      m.sender,
      m.seqNum,
      m.bonds,
      m.justifications.toSet,
      Set(),
      seen
    )
}
