package coop.rchain.casper.api

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.rholang.RuntimeManager
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.models.{BlockMetadata, FringeData}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.testing.scalatest.MonixTaskTest
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito, Mockito}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap

// See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
class BlocksResponseAPITest
    extends AsyncFlatSpec
    with MonixTaskTest
    with Matchers
    with EitherValues
    with BlockGenerator
    with BlockDagStorageFixture
    with BlockApiFixture
    with IdiomaticMockito
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

  private def createDagWith8Blocks[F[_]: Sync: BlockStore: BlockDagStorage] =
    for {
      genesis <- createGenesis[F](bonds = bonds)
      b2 <- createBlock[F](
             v2,
             bonds,
             Seq(genesis.blockHash)
           )
      b3 <- createBlock[F](
             v1,
             bonds,
             Seq(genesis.blockHash)
           )
      b4 <- createBlock[F](
             v3,
             bonds,
             Seq(genesis.blockHash, b2.blockHash)
           )
      b5 <- createBlock[F](
             v2,
             bonds,
             Seq(b3.blockHash, b2.blockHash, genesis.blockHash)
           )
      b6 <- createBlock[F](
             v1,
             bonds,
             Seq(b3.blockHash, b2.blockHash, b4.blockHash)
           )
      b7 <- createBlock[F](
             v3,
             bonds,
             Seq(b3.blockHash, b5.blockHash, b4.blockHash)
           )
      b8 <- createBlock[F](
             v2,
             bonds,
             Seq(b6.blockHash, b5.blockHash, b4.blockHash)
           )
    } yield List(genesis, b2, b3, b4, b5, b6, b7, b8)

  "getBlocks" should "return all blocks" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = createMocks[Task]

    for {
      blocks         <- createDagWith8Blocks[Task]
      genesis        = blocks.head
      blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit)
      _              = Mockito.clearInvocations(blockStore, blockDagStorage)
      blocksResponse <- blockApi.getBlocks(10)
    } yield {
      blocksResponse shouldBe 'right
      blocksResponse.value.length shouldBe 8

      blocks.map { b =>
        blockStore.get(Seq(b.blockHash)) wasCalled once
      }
      verifyNoMoreInteractions(blockStore)

      blockDagStorage.insert(*, *) wasNever called
      blockDagStorage.getRepresentation wasCalled once
    }
  }

  it should "return until depth" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = createMocks[Task]

    for {
      blocks         <- createDagWith8Blocks[Task]
      genesis        = blocks.head
      blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit)
      _              = Mockito.clearInvocations(blockStore, blockDagStorage)
      blocksResponse <- blockApi.getBlocks(2)
    } yield {
      blocksResponse shouldBe 'right
      blocksResponse.value.length shouldBe 3

      blocks.takeRight(3).map { b =>
        blockStore.get(Seq(b.blockHash)) wasCalled once
      }
      verifyNoMoreInteractions(blockStore)

      blockDagStorage.insert(*, *) wasNever called
      blockDagStorage.getRepresentation wasCalled once
    }
  }

  "getBlocksByHeights" should "return blocks between startBlockNumber and endBlockNumber" in {
    implicit val (blockStore, blockDagStorage, runtimeManager) = createMocks[Task]

    for {
      blocks         <- createDagWith8Blocks[Task]
      genesis        = blocks.head
      blockApi       <- createBlockApi[Task](genesis.shardId, maxBlockLimit)
      _              = Mockito.clearInvocations(blockStore, blockDagStorage)
      blocksResponse <- blockApi.getBlocksByHeights(2, 5)
    } yield {
      blocksResponse shouldBe 'right
      blocksResponse.value shouldBe blocks.takeRight(5).map(BlockApi.getLightBlockInfo)

      blocks.takeRight(5).map { b =>
        blockStore.get(Seq(b.blockHash)) wasCalled once
      }
      verifyNoMoreInteractions(blockStore)

      blockDagStorage.insert(*, *) wasNever called
      blockDagStorage.getRepresentation wasCalled once
    }
  }

  private def createMocks[F[_]: Sync] =
    (createBlockStore, createBlockDagStorage, mock[RuntimeManager[F]])

  private def createBlockStore[F[_]: Sync]: BlockStore[F] = {
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

  private def createBlockDagStorage[F[_]: Sync]: BlockDagStorage[F] = {
    val genesisHash = RuntimeManager.emptyStateHashFixed

    val state = Ref.unsafe[F, DagRepresentation](
      DagRepresentation(
        Set(),
        Map(),
        SortedMap(),
        DagMessageState(),
        Map(
          Set(genesisHash) -> FringeData(
            FringeData.fringeHash(Set.empty),
            Set.empty,
            Set.empty,
            genesisHash.toBlake2b256Hash,
            Set.empty,
            Set.empty,
            Set.empty
          )
        )
      )
    )
    val bds = mock[BlockDagStorage[F]]

    bds.insert(any, any) answers { (bmd: BlockMetadata, b: BlockMessage) =>
      state.update { s =>
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
