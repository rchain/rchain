//package coop.rchain.casper.api
//
//import cats.effect.Sync
//import cats.implicits._
//import com.google.protobuf.ByteString
//import coop.rchain.blockstorage.BlockStore
//import coop.rchain.casper.engine.Engine
//import coop.rchain.casper.SafetyOracle
//import coop.rchain.casper.helper._
//import coop.rchain.casper.helper.BlockGenerator._
//import coop.rchain.casper.protocol._
//import coop.rchain.casper.util.GenesisBuilder._
//import coop.rchain.casper.util.ProtoUtil
//import coop.rchain.casper.util.ConstructDeploy.basicDeployData
//import coop.rchain.casper.batch2.EngineWithCasper
//import coop.rchain.crypto.signatures.Secp256k1
//import coop.rchain.metrics.Metrics
//import coop.rchain.shared.{Cell, Log}
//import coop.rchain.shared.scalatestcontrib._
//import monix.eval.Task
//import org.scalatest.{EitherValues, FlatSpec, Matchers}
//import monix.execution.Scheduler.Implicits.global
//
//import scala.collection.immutable.Map
//
//class LastFinalizedAPITest
//    extends FlatSpec
//    with Matchers
//    with EitherValues
//    with BlockGenerator
//    with BlockDagStorageFixture {
//  val genesisParameters = buildGenesisParameters(bondsFunction = _.zip(List(10L, 10L, 10L)).toMap)
//  val genesisContext    = buildGenesis(genesisParameters)
//
//  implicit val metricsEff = new Metrics.MetricsNOP[Task]
//
//  def isFinalized(block: BlockMessage)(engineCell: Cell[Task, Engine[Task]])(
//      implicit blockStore: BlockStore[Task],
//      safetyOracle: SafetyOracle[Task],
//      log: Log[Task]
//  ): Task[Boolean] =
//    BlockAPI
//      .isFinalized[Task](ProtoUtil.hashString(block))(
//        Sync[Task],
//        engineCell,
//        safetyOracle,
//        blockStore,
//        log
//      )
//      .map(
//        _.right.value
//      )
//
//  /*
//   * DAG Looks like this:
//   *
//   *           b7
//   *           |
//   *           b6
//   *           |
//   *           b5 <- last finalized block
//   *         / |
//   *        |  b4
//   *        |  |
//   *       b2  b3
//   *         \ |
//   *           b1
//   *           |
//   *         genesis
//   */
//  "isFinalized" should "return true for ancestors of last finalized block" ignore effectTest {
//    TestNode.networkEff(genesisContext, networkSize = 3).use {
//      case nodes @ n1 +: n2 +: n3 +: Seq() =>
//        import n1.{blockStore, cliqueOracleEffect, logEff}
//        val engine = new EngineWithCasper[Task](n1.casperEff)
//        for {
//          produceDeploys <- (0 until 7).toList.traverse(i => basicDeployData[Task](i))
//
//          b1 <- n1.propagateBlock(produceDeploys(0))(nodes: _*)
//          b2 <- n2.publishBlock(produceDeploys(1))()
//          b3 <- n3.propagateBlock(produceDeploys(2))(n1)
//          b4 <- n1.propagateBlock(produceDeploys(3))(nodes: _*)
//          b5 <- n2.propagateBlock(produceDeploys(4))(nodes: _*)
//          b6 <- n1.propagateBlock(produceDeploys(5))(nodes: _*)
//          b7 <- n2.propagateBlock(produceDeploys(6))(nodes: _*)
//
//          lastFinalizedBlock <- n1.casperEff.lastFinalizedBlock
//          _                  = lastFinalizedBlock shouldBe b5
//
//          engineCell <- Cell.mvarCell[Task, Engine[Task]](engine)
//          // Checking if last finalized block is finalized
//          _ <- isFinalized(b5)(engineCell) shouldBeF true
//          // Checking if parent of last finalized block is finalized
//          _ <- isFinalized(b4)(engineCell) shouldBeF true
//          // Checking if secondary parent of last finalized block is finalized
//          _ <- isFinalized(b2)(engineCell) shouldBeF true
//        } yield ()
//    }
//  }
//
//  /*
//   * DAG Looks like this:
//   *
//   *           b5
//   *             \
//   *              b4
//   *             /
//   *        b7 b3 <- last finalized block
//   *        |    \
//   *        b6    b2
//   *          \  /
//   *           b1
//   *       [n3 n1 n2]
//   *           |
//   *         genesis
//   */
//  it should "return false for children, uncles and cousins of last finalized block" in effectTest {
//    TestNode.networkEff(genesisContext, networkSize = 3).use {
//      case nodes @ n1 +: n2 +: n3 +: Seq() =>
//        import n1.{blockStore, cliqueOracleEffect, logEff}
//        val engine = new EngineWithCasper[Task](n1.casperEff)
//        for {
//          produceDeploys <- (0 until 7).toList.traverse(i => basicDeployData[Task](i))
//
//          b1 <- n1.propagateBlock(produceDeploys(0))(nodes: _*)
//          b2 <- n2.propagateBlock(produceDeploys(1))(n1)
//          b3 <- n1.propagateBlock(produceDeploys(2))(n2)
//          b4 <- n2.propagateBlock(produceDeploys(3))(n1)
//          b5 <- n1.propagateBlock(produceDeploys(4))(n2)
//          _  <- n3.shutoff() // n3 misses b2, b3, b4, b5
//          b6 <- n3.propagateBlock(produceDeploys(5))(nodes: _*)
//          b7 <- n3.propagateBlock(produceDeploys(6))(nodes: _*)
//
//          lastFinalizedBlock <- n1.casperEff.lastFinalizedBlock
//          _                  = lastFinalizedBlock shouldBe b3
//
//          engineCell <- Cell.mvarCell[Task, Engine[Task]](engine)
//          // Checking if child of last finalized block is finalized
//          _ <- isFinalized(b4)(engineCell) shouldBeF false
//          // Checking if uncle of last finalized block is finalized
//          _ <- isFinalized(b6)(engineCell) shouldBeF false
//          // Checking if cousin of last finalized block is finalized
//          _ <- isFinalized(b7)(engineCell) shouldBeF false
//        } yield ()
//    }
//  }
//}
