package coop.rchain.casper.api

import cats.syntax.all._
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ConstructDeploy.basicDeployData
import coop.rchain.casper.util.GenesisBuilder._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.metrics.Metrics
import coop.rchain.shared.scalatestcontrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{EitherValues, FlatSpec, Matchers}

// TODO finalizer test for multiparent
class LastFinalizedAPITest
    extends FlatSpec
    with Matchers
    with EitherValues
    with BlockGenerator
    with BlockDagStorageFixture
    with BlockApiFixture {
  val genesisParameters = buildGenesisParameters(bondsFunction = _.zip(List(10L, 10L, 10L)).toMap)
  val genesisContext    = buildGenesis(genesisParameters)

  implicit val metricsEff = new Metrics.MetricsNOP[Task]

  def isFinalized(node: TestNode[Task])(block: BlockMessage): Task[Boolean] =
    for {
      blockApi <- createBlockApi(node)
      res      <- blockApi.isFinalized(ProtoUtil.hashString(block))
    } yield res.right.value

  /*
   * DAG Looks like this:
   *
   *           b7
   *           |
   *           b6
   *           |
   *           b5 <- last finalized block
   *         / |
   *        |  b4
   *        |  |
   *       b2  b3
   *         \ |
   *           b1
   *           |
   *         genesis
   */
  "isFinalized" should "return true for ancestors of last finalized block" ignore effectTest {
    TestNode.networkEff(genesisContext, networkSize = 3).use {
      case nodes @ n1 +: n2 +: n3 +: Seq() =>
        for {
          produceDeploys <- (0 until 7).toList.traverse(i => basicDeployData[Task](i))

          b1 <- n1.propagateBlock(produceDeploys(0))(nodes: _*)
          b2 <- n2.publishBlock(produceDeploys(1))()
          b3 <- n3.propagateBlock(produceDeploys(2))(n1)
          b4 <- n1.propagateBlock(produceDeploys(3))(nodes: _*)
          b5 <- n2.propagateBlock(produceDeploys(4))(nodes: _*)
          b6 <- n1.propagateBlock(produceDeploys(5))(nodes: _*)
          b7 <- n2.propagateBlock(produceDeploys(6))(nodes: _*)

          lastFinalizedBlock <- n1.lastFinalizedBlock
          _                  = lastFinalizedBlock shouldBe b5

          // Checking if last finalized block is finalized
          _ <- isFinalized(n1)(b5) shouldBeF true
          // Checking if parent of last finalized block is finalized
          _ <- isFinalized(n1)(b4) shouldBeF true
          // Checking if secondary parent of last finalized block is finalized
          _ <- isFinalized(n1)(b2) shouldBeF true
        } yield ()
    }
  }

  /*
   * DAG Looks like this:
   *
   *           b5
   *             \
   *              b4
   *             /
   *        b7 b3 <- last finalized block
   *        |    \
   *        b6    b2
   *          \  /
   *           b1
   *       [n3 n1 n2]
   *           |
   *         genesis
   */
  it should "return false for children, uncles and cousins of last finalized block" ignore effectTest {
    TestNode.networkEff(genesisContext, networkSize = 3).use {
      case nodes @ n1 +: n2 +: n3 +: Seq() =>
        for {
          produceDeploys <- (0 until 7).toList.traverse(
                             i =>
                               basicDeployData[Task](
                                 i,
                                 shardId = genesisContext.genesisBlock.shardId
                               )
                           )

          b1 <- n1.propagateBlock(produceDeploys(0))(nodes: _*)
          b2 <- n2.propagateBlock(produceDeploys(1))(n1)
          b3 <- n1.propagateBlock(produceDeploys(2))(n2)
          b4 <- n2.propagateBlock(produceDeploys(3))(n1)
          b5 <- n1.propagateBlock(produceDeploys(4))(n2)
          _  <- n3.shutoff() // n3 misses b2, b3, b4, b5
          b6 <- n3.propagateBlock(produceDeploys(5))(nodes: _*)
          b7 <- n3.propagateBlock(produceDeploys(6))(nodes: _*)

          lastFinalizedBlock <- n1.lastFinalizedBlock
          _                  = lastFinalizedBlock shouldBe b3

          // Checking if child of last finalized block is finalized
          _ <- isFinalized(n1)(b4) shouldBeF false
          // Checking if uncle of last finalized block is finalized
          _ <- isFinalized(n1)(b6) shouldBeF false
          // Checking if cousin of last finalized block is finalized
          _ <- isFinalized(n1)(b7) shouldBeF false
        } yield ()
    }
  }
}
