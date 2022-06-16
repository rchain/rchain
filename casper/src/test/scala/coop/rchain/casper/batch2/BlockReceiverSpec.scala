package coop.rchain.casper.blocks.proposer

import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.casper.blocks.{BlockReceiver, BlockReceiverState}
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.syntax._
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Log
import monix.eval.Task
import monix.testing.scalatest.MonixTaskTest
import org.scalatest.Assertion
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class BlockReceiverSpec extends AsyncFlatSpec with MonixTaskTest with Matchers {
  implicit val logEff: Log[Task]            = Log.log[Task]
  implicit val timeEff: LogicalTime[Effect] = new LogicalTime[Effect]
  private val genesis                       = buildGenesis()

  import fs2._

  private def withBlockReceiverEnv(shardId: String)(
      f: Stream[Task, BlockHash] => Task[Assertion]
  ): Task[Assertion] = TestNode.standaloneEff(genesis).use { node =>
    implicit val (bds, br, bs) =
      (node.blockDagStorage, node.blockRetrieverEffect, node.blockStore)

    for {
      state                 <- Ref[Task].of(BlockReceiverState[BlockHash])
      block                 <- makeBlock(node)
      incomingBlockStream   = Stream[Task, BlockMessage](block)
      validatedBlocksStream = Stream[Task, BlockMessage]()
      br                    <- BlockReceiver(state, incomingBlockStream, validatedBlocksStream, shardId)
      res                   <- f(br)
    } yield res
  }

  private def makeBlock(node: TestNode[Task]): Task[BlockMessage] =
    ConstructDeploy
      .sourceDeployNowF("new x in { x!(0) }", shardId = genesis.genesisBlock.shardId) >>= (node
      .createBlockUnsafe(_))

  it should "pass correct block to output stream" in {
    withBlockReceiverEnv("root") { outStream =>
      outStream.take(1).compile.toList.map(outList => outList.length shouldBe 1)
    }
  }

  it should "discard block with invalid shard name" in {
    // Provided to BlockReceiver shard name ("test") is differ from block's shard name ("root" by default)
    // So block should be rejected and output stream should never take block
    withBlockReceiverEnv("test") { outStream =>
      outStream
        .take(1)
        .interruptAfter(1.second)
        .compile
        .toList
        .map(outList => outList shouldBe empty)
    }
  }
}
