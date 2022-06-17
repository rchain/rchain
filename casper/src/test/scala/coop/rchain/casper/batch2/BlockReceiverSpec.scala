package coop.rchain.casper.blocks.proposer

import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.casper.blocks.{BlockReceiver, BlockReceiverState}
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.casper.util.scalatest.Fs2StreamMatchers
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.syntax._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Log
import fs2.concurrent.Queue
import monix.eval.Task
import monix.testing.scalatest.MonixTaskTest
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class BlockReceiverSpec
    extends AsyncFlatSpec
    with MonixTaskTest
    with Matchers
    with Fs2StreamMatchers {
  implicit val logEff: Log[Task]            = Log.log[Task]
  implicit val timeEff: LogicalTime[Effect] = new LogicalTime[Effect]
  private val genesis                       = buildGenesis()

  import fs2._

  private def withBlockReceiverEnv(shardId: String)(
      f: (Queue[Task, BlockMessage], Stream[Task, BlockHash], TestNode[Task]) => Task[Assertion]
  ): Task[Assertion] = TestNode.standaloneEff(genesis).use { node =>
    implicit val (bds, br, bs) =
      (node.blockDagStorage, node.blockRetrieverEffect, node.blockStore)

    for {
      state                 <- Ref[Task].of(BlockReceiverState[BlockHash])
      incomingBlockQueue    <- Queue.unbounded[Task, BlockMessage]
      incomingBlockStream   = incomingBlockQueue.dequeue
      validatedBlocksStream = Stream[Task, BlockMessage]()
      br                    <- BlockReceiver(state, incomingBlockStream, validatedBlocksStream, shardId)
      res                   <- f(incomingBlockQueue, br, node)
    } yield res
  }

  private def makeDeploy =
    ConstructDeploy.sourceDeployNowF("new x in { x!(0) }", shardId = genesis.genesisBlock.shardId)

  private def makeBlock(node: TestNode[Task]): Task[BlockMessage] =
    makeDeploy >>= (node.createBlockUnsafe(_))

  private def addBlock(node: TestNode[Task]): Task[BlockMessage] = makeDeploy >>= (node.addBlock(_))

  it should "pass correct block to output stream" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, outStream, node) =>
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
      case (incomingQueue, outStream, node) =>
        for {
          block <- makeBlock(node)
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }

  it should "discard block with invalid block hash" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, outStream, node) =>
        for {
          block <- makeBlock(node).map(_.copy(blockHash = "abc".unsafeHexToByteString))
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }

  it should "discard block with invalid signature" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, outStream, node) =>
        for {
          block <- makeBlock(node).map(_.copy(sig = "abc".unsafeHexToByteString))
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }

  it should "discard known block" in {
    withBlockReceiverEnv("root") {
      case (incomingQueue, outStream, node) =>
        for {
          block <- addBlock(node)
          _     <- incomingQueue.enqueue1(block)
        } yield outStream should notEmit
    }
  }
}
