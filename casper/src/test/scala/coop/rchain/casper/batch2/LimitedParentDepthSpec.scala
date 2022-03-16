package coop.rchain.casper.batch2

import cats.instances.list._
import cats.syntax.traverse._
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.util.ConstructDeploy.basicDeployData
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{FlatSpec, Matchers}

class LimitedParentDepthSpec extends FlatSpec with Matchers {
  implicit val scheduler = Scheduler.fixedPool("limited-parent-depth-scheduler", 2)
  implicit val timeEff   = new LogicalTime[Task]

  val genesisContext   = buildGenesis()
  private val SHARD_ID = genesisContext.genesisBlock.shardId

  "Estimator" should "obey present parent depth limitation" in {
    TestNode
      .networkEff(genesisContext, networkSize = 2, maxParentDepth = Some(2))
      .use {
        case nodes @ n1 +: n2 +: Seq() =>
          for {
            produceDeploys <- (0 until 6).toList
                               .traverse(i => basicDeployData[Task](i, shardId = SHARD_ID))

            b1 <- n1.propagateBlock(produceDeploys(0))()
            b2 <- n2.propagateBlock(produceDeploys(1))(nodes: _*)
            b4 <- n2.propagateBlock(produceDeploys(2))(nodes: _*)
            b4 <- n2.propagateBlock(produceDeploys(3))(nodes: _*)
            b5 <- n2.propagateBlock(produceDeploys(4))(nodes: _*)
            b6 <- n1.propagateBlock(produceDeploys(5))(nodes: _*)
          } yield b6.header.parentsHashList shouldBe List(b5.blockHash)
      }
  }

  it should "obey absent parent depth limitation" in {
    TestNode
      .networkEff(genesisContext, networkSize = 2, maxParentDepth = None)
      .use {
        case nodes @ n1 +: n2 +: Seq() =>
          for {
            produceDeploys <- (0 until 6).toList
                               .traverse(i => basicDeployData[Task](i, shardId = SHARD_ID))

            b1 <- n1.propagateBlock(produceDeploys(0))()
            b2 <- n2.propagateBlock(produceDeploys(1))(nodes: _*)
            b4 <- n2.propagateBlock(produceDeploys(2))(nodes: _*)
            b4 <- n2.propagateBlock(produceDeploys(3))(nodes: _*)
            b5 <- n2.propagateBlock(produceDeploys(4))(nodes: _*)
            b6 <- n1.propagateBlock(produceDeploys(5))(nodes: _*)
          } yield b6.header.parentsHashList shouldBe List(b1.blockHash, b5.blockHash)
      }
  }
}
