package coop.rchain.casper.batch2

import cats.Applicative
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagMessageState, DagRepresentation}
import coop.rchain.casper.blocks.{BlockReceiver, BlockReceiverState}
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.casper.util.scalatest.Fs2StreamMatchers
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.syntax._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Log
import fs2.concurrent.Queue
import monix.eval.Task
import monix.testing.scalatest.MonixTaskTest
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap

class BlockReceiverSpec
    extends AsyncFlatSpec
    with MonixTaskTest
    with Matchers
    with Fs2StreamMatchers {
  implicit val logEff: Log[Task]            = Log.log[Task]
  implicit val timeEff: LogicalTime[Effect] = new LogicalTime[Effect]
  private val genesis                       = buildGenesis()

  import fs2._

  final class BdsMock[F[_]: Applicative] extends BlockDagStorage[F] {
    override def getRepresentation: F[DagRepresentation] =
      DagRepresentation(Set(), Map(), SortedMap(), DagMessageState(), Map()).pure[F]
    override def insertNew(
        blockMetadata: BlockMetadata,
        block: BlockMessage
    ): F[DagRepresentation] = ???
    override def insert(
        block: BlockMessage,
        invalid: Boolean,
        approved: Boolean
    ): F[DagRepresentation]                                                  = ???
    override def lookup(blockHash: BlockHash): F[Option[BlockMetadata]]      = ???
    override def lookupByDeployId(blockHash: DeployId): F[Option[BlockHash]] = ???
    override def addDeploy(d: Signed[DeployData]): F[Unit]                   = ???
    override def pooledDeploys: F[Map[DeployId, Signed[DeployData]]]         = ???
    override def containsDeployInPool(deployId: DeployId): F[Boolean]        = ???
  }

  private def withBlockReceiverEnv(shardId: String)(
      f: (
          Queue[Task, BlockMessage],
          Queue[Task, BlockMessage],
          Stream[Task, BlockHash],
          TestNode[Task]
      ) => Task[Assertion]
  ): Task[Assertion] = TestNode.standaloneEff(genesis).use { node =>
    implicit val (_, br, bs) =
      (node.blockDagStorage, node.blockRetrieverEffect, node.blockStore)
    implicit val bds: BdsMock[Task] = new BdsMock[Task]

    for {
      state                 <- Ref[Task].of(BlockReceiverState[BlockHash])
      incomingBlockQueue    <- Queue.unbounded[Task, BlockMessage]
      incomingBlockStream   = incomingBlockQueue.dequeue
      validatedBlocksQueue  <- Queue.unbounded[Task, BlockMessage]
      validatedBlocksStream = validatedBlocksQueue.dequeue
      br                    <- BlockReceiver(state, incomingBlockStream, validatedBlocksStream, shardId)
      res                   <- f(incomingBlockQueue, validatedBlocksQueue, br, node)
    } yield res
  }

  private def makeDeploy =
    ConstructDeploy.sourceDeployNowF("new x in { x!(0) }", shardId = genesis.genesisBlock.shardId)

  private def makeBlock(
      node: TestNode[Task],
      justifications: List[BlockHash] = List()
  ): Task[BlockMessage] =
    for {
      deploy      <- makeDeploy
      block       <- node.createBlockUnsafe(deploy).map(_.copy(justifications = justifications))
      signedBlock = node.validatorIdOpt.map(_.signBlock(block)).getOrElse(block)
    } yield signedBlock

  private def addBlock(node: TestNode[Task]): Task[BlockMessage] = makeDeploy >>= (node.addBlock(_))

  it should "pass correct block to output stream" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, _, outStream, node) =>
        for {
          block   <- makeBlock(node)
          _       <- incomingQueue.enqueue1(block)
          outList <- outStream.take(1).compile.toList
        } yield outList.length shouldBe 1
    }
  }

  it should "discard block with invalid shard name" in {
    // Provided to BlockReceiver shard name ("test") is differ from block's shard name ("root" by default)
    // So block should be rejected and output stream should never take block
    withBlockReceiverEnv("test") {
      case (incomingQueue, _, outStream, node) =>
        for {
          block <- makeBlock(node)
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }

  it should "discard block with invalid block hash" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, _, outStream, node) =>
        for {
          block <- makeBlock(node).map(_.copy(blockHash = "abc".unsafeHexToByteString))
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }

  it should "discard block with invalid signature" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, _, outStream, node) =>
        for {
          block <- makeBlock(node).map(_.copy(sig = "abc".unsafeHexToByteString))
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }

  it should "discard known block" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, _, outStream, node) =>
        for {
          block <- addBlock(node)
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }

  "BlockReceiver" should "pass to output blocks with resolved dependencies" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, validatedQueue, outStream, node) =>
        for {
          // Received a parent with an empty list of justifications and its child
          a1 <- makeBlock(node)
          a2 <- makeBlock(node, List(a1.blockHash))

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
}
