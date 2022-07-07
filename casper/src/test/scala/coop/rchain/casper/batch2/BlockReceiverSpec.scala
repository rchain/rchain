package coop.rchain.casper.batch2

import cats.Applicative
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagMessageState, DagRepresentation}
import coop.rchain.casper.ValidatorIdentity
import coop.rchain.casper.blocks.{BlockReceiver, BlockReceiverState}
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.protocol.{BlockMessage, BlockMessageProto}
import coop.rchain.casper.util.scalatest.Fs2StreamMatchers
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.syntax._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Log
import fs2.concurrent.Queue
import monix.eval.Task
import monix.testing.scalatest.MonixTaskTest
import org.mockito.cats.IdiomaticMockitoCats
import org.mockito.{ArgumentMatchersSugar, IdiomaticMockito}
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap

class BlockReceiverSpec
    extends AsyncFlatSpec
    with MonixTaskTest
    with Matchers
    with Fs2StreamMatchers
    with IdiomaticMockito
    with IdiomaticMockitoCats
    with ArgumentMatchersSugar {
  implicit val logEff: Log[Task]            = Log.log[Task]
  implicit val timeEff: LogicalTime[Effect] = new LogicalTime[Effect]

  implicit val blockDagStorageEff: BlockDagStorage[Task] = blockDagStorageMock[Task]()
  implicit val blockRetrieverEff: BlockRetriever[Task]   = blockRetrieverMock[Task]()
  implicit val blockStoreEff: BlockStore[Task]           = blockStoreMock[Task]()

  it should "pass correct block to output stream" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, _, outStream) =>
        for {
          block   <- makeBlock()
          _       <- incomingQueue.enqueue1(block)
          outList <- outStream.take(1).compile.toList
        } yield outList.length shouldBe 1
    }
  }

  it should "discard block with invalid shard name" in {
    // Provided to BlockReceiver shard name ("test") is differ from block's shard name ("root" by default)
    // So block should be rejected and output stream should never take block
    withBlockReceiverEnv("test") {
      case (incomingQueue, _, outStream) =>
        for {
          block <- makeBlock()
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }

  it should "discard block with invalid block hash" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, _, outStream) =>
        for {
          block <- makeBlock().map(_.copy(blockHash = "abc".unsafeHexToByteString))
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }

  it should "discard block with invalid signature" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, _, outStream) =>
        for {
          block <- makeBlock().map(_.copy(sig = "abc".unsafeHexToByteString))
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }

  it should "discard known block" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, _, outStream) =>
        for {
          block <- addBlock()
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }

  it should "pass to output blocks with resolved dependencies" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, validatedQueue, outStream) =>
        for {
          // Received a parent with an empty list of justifications and its child
          a1 <- makeBlock()
          a2 <- makeBlock(List(a1.blockHash))

          // Put the parent and child in the input queue
          _ <- incomingQueue.enqueue1(a2)
          _ <- incomingQueue.enqueue1(a1)

          // Dependencies of the child (its parent) have not yet been resolved,
          // so only the parent goes to the output queue, since it has no dependencies
          a1InOutQueue <- outStream.take(1).compile.toList.map(_.head)

          // A1 is now validated (e.g. in BlockProcessor)
          _ <- validatedQueue.enqueue1(a1)

          // All dependencies of child A2 are resolved, so it also goes to the output queue
          a2InOutQueue <- outStream.take(1).compile.toList.map(_.head)
        } yield {
          a1InOutQueue shouldBe a1.blockHash
          a2InOutQueue shouldBe a2.blockHash
        }
    }
  }

  private def blockDagStorageMock[F[_]: Applicative](): BlockDagStorage[F] = {
    val emptyDag = DagRepresentation(Set(), Map(), SortedMap(), DagMessageState(), Map())
    mock[BlockDagStorage[F]].getRepresentation returnsF emptyDag
  }

  private def blockRetrieverMock[F[_]: Applicative](): BlockRetriever[F] =
    mock[BlockRetriever[F]].ackReceived(*) returns ().pure[F]

  private def blockStoreMock[F[_]: Sync: Applicative](): BlockStore[F] = {
    val state  = Ref.unsafe[F, Map[BlockHash, BlockMessage]](Map())
    val bsMock = mock[BlockStore[F]]
    bsMock.contains(*) answers { keys: Seq[BlockHash] =>
      state.get.map(s => Seq(s.contains(keys.head)))
    }
    bsMock.put(*) answers { kvPairs: Seq[(BlockHash, BlockMessage)] =>
      state.update(s => kvPairs.foldLeft(s) { case (acc, item) => acc + item })
    }
    bsMock
  }

  import fs2._

  private def withBlockReceiverEnv(shardId: String)(
      f: (
          Queue[Task, BlockMessage],
          Queue[Task, BlockMessage],
          Stream[Task, BlockHash]
      ) => Task[Assertion]
  ): Task[Assertion] =
    for {
      state                 <- Ref[Task].of(BlockReceiverState[BlockHash])
      incomingBlockQueue    <- Queue.unbounded[Task, BlockMessage]
      incomingBlockStream   = incomingBlockQueue.dequeue
      validatedBlocksQueue  <- Queue.unbounded[Task, BlockMessage]
      validatedBlocksStream = validatedBlocksQueue.dequeue
      br                    <- BlockReceiver(state, incomingBlockStream, validatedBlocksStream, shardId)
      res                   <- f(incomingBlockQueue, validatedBlocksQueue, br)
    } yield res

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

  private def makeBlock(
      justifications: List[BlockHash] = List()
  ): Task[BlockMessage] = {
    val (privateKey, pubKey) = Secp256k1.newKeyPair
    for {
      block <- makeDefaultBlock
                .copy(sender = pubKey.bytes.toByteString, justifications = justifications)
                .pure[Task]
      validatorId = ValidatorIdentity(privateKey)
      signedBlock = validatorId.signBlock(block)
    } yield signedBlock
  }

  private def addBlock(): Task[BlockMessage] =
    for {
      block <- makeBlock()
      _     <- BlockStore[Task].put(Seq((block.blockHash, block)))
    } yield block

}
