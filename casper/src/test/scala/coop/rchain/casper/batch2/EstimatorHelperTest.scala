package coop.rchain.casper.batch2

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.{BlockDagStorage, IndexedBlockDagStorage}
import coop.rchain.casper.EstimatorHelper
import coop.rchain.casper.EstimatorHelper.conflicts
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator, TestNode}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.ConstructDeploy._
import coop.rchain.casper.util.GenesisBuilder
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockMetadata
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.scalatestcontrib._
import coop.rchain.shared.{Log, Time}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

class EstimatorHelperTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture {

  implicit val log: Log[Task]            = new Log.NOPLog[Task]
  implicit val timeEff: Time[Task]       = new LogicalTime[Task]
  implicit val metricsEff: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task]      = NoopSpan[Task]()

  val genesisContext = GenesisBuilder.buildGenesis()
  val genesis        = genesisContext.genesisBlock
  val channelsHash   = "Abc45678123dsbnxmajq124jdkamsk23"

  /*
   * DAG Looks like this:
   *
   *       b9      b10
   *        \      /
   *        b7   b8
   *          \  /
   *           b6
   *           / \
   *      b4  /   \  b5
   *       | /     \ |
   *       b2       b3
   *        \       /
   *         genesis
   */

  "Conflicts" should "be also detected when they occur in uncommon ancestor blocks" ignore effectTest {

    TestNode.networkEff(genesisContext, networkSize = 4).use {
      case n1 +: n2 +: n3 +: n4 +: _ =>
        implicit val blockStore = n4.blockStore

        implicit def blockMessageToBlockMetadata(b: BlockMessage): BlockMetadata =
          BlockMetadata.fromBlock(b, invalid = false)

        for {
          produceDeploys <- (0 until 6).toList.traverse(i => basicSendDeployData[Task](i))
          consumeDeploys <- (0 until 6).toList
                             .traverse(i => basicReceiveDeployData[Task](i))

          b2 <- n1.addBlock(produceDeploys(0))
          b3 <- n2.addBlock(produceDeploys(1))
          _  <- n3.syncWith(n1, n2)

          b4 <- n1.addBlock(produceDeploys(2))
          b5 <- n2.addBlock(consumeDeploys(2))
          b6 <- n3.addBlock(produceDeploys(2))
          _  <- n4.syncWith(n1, n2, n3)

          b7  <- n3.addBlock(produceDeploys(3))
          b8  <- n4.addBlock(produceDeploys(5))
          b9  <- n3.addBlock(consumeDeploys(5))
          b10 <- n4.addBlock(produceDeploys(4))

          _   <- n4.syncWith(n3)
          dag <- n4.blockDagStorage.getRepresentation

          _ <- conflicts[Task](b2, b3, dag) shouldBeF false
          // b4 conflicts with b5 directly
          _ <- conflicts[Task](b4, b5, dag) shouldBeF true
          _ <- conflicts[Task](b6, b6, dag) shouldBeF false
          _ <- conflicts[Task](b6, b9, dag) shouldBeF false
          _ <- conflicts[Task](b7, b8, dag) shouldBeF false
          _ <- conflicts[Task](b7, b10, dag) shouldBeF false
          // b8 conflicts with b9 directly
          _ <- conflicts[Task](b8, b9, dag) shouldBeF true
          // b9 conflicts indirectly with b10 due to the fact that b8 is the latter's ancestor
          _ <- conflicts[Task](b9, b10, dag) shouldBeF true
        } yield ()
    }
  }

  "Blocks" should "conflict if their deploys conflict in the deployLog" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      testConflict[Task] { deploy =>
        deploy.copy(
          deployLog = Vector(
            produce(ByteString.copyFromUtf8(channelsHash))
          )
        )
      } { deploy =>
        deploy.copy(deployLog = Vector(consume(ByteString.copyFromUtf8(channelsHash))))
      }
  }

  private def testConflict[F[_]: Sync: BlockStore: IndexedBlockDagStorage: Time: Log: Span](
      deployMod1: ProcessedDeploy => ProcessedDeploy
  )(
      deployMod2: ProcessedDeploy => ProcessedDeploy
  ): F[Unit] = {
    implicit def blockMessageToBlockMetadata(b: BlockMessage): BlockMetadata =
      BlockMetadata.fromBlock(b, invalid = false)

    for {
      genesis <- createGenesis[F]()
      deployA <- basicProcessedDeploy[F](1).map(deployMod1)
      a       <- createBlock[F](Seq(genesis.blockHash), genesis, deploys = Seq(deployA))
      deployB <- basicProcessedDeploy[F](2).map(deployMod2)
      b       <- createBlock[F](Seq(genesis.blockHash), genesis, deploys = Seq(deployB))
      dag     <- BlockDagStorage[F].getRepresentation

      _              <- conflicts[F](a, b, dag) shouldBeF true
      nonconflicting <- EstimatorHelper.chooseNonConflicting(Seq(a, b).map(_.blockHash), dag)
      _              = assert(nonconflicting == List[BlockMetadata](a))
    } yield ()
  }

  private def produce(channelsHash: ByteString) =
    ProduceEvent(
      channelsHash,
      hash = ByteString.copyFromUtf8("Asdfg753213fdsadfueias9fje35mv43"),
      persistent = false,
      timesRepeated = 0
    )

  private def consume(channelsHash: ByteString) =
    ConsumeEvent(
      List(channelsHash),
      hash = ByteString.copyFromUtf8("Asdfg753213fdsadfueias9fje35mv43"),
      persistent = false
    )
}
