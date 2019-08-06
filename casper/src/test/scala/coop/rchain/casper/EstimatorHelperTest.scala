package coop.rchain.casper

import cats.Monad
import cats.effect.Resource
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagStorage, BlockStore, IndexedBlockDagStorage}
import coop.rchain.casper.EstimatorHelper.conflicts
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator, HashSetCasperTestNode}
import coop.rchain.casper.protocol.Event.EventInstance.{Consume, Produce}
import coop.rchain.casper.protocol.{ConsumeEvent, Event, ProcessedDeploy, ProduceEvent}
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy.{basicDeployData, basicProcessedDeploy}
import coop.rchain.casper.util.GenesisBuilder
import coop.rchain.casper.util.rholang.{Resources, RuntimeManager}
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
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

  "Blocks" should "conflict if they use the same deploys in different histories" in effectTest {

    HashSetCasperTestNode.networkEff(genesisContext, networkSize = 4).use {
      case n1 +: n2 +: n3 +: n4 +: _ =>
        implicit val blockStore = n4.blockStore

        for {
          deploys <- (0 until 6).toList.traverse(i => basicDeployData[Task](i))

          b2 <- n1.addBlock(deploys(0))
          b3 <- n2.addBlock(deploys(1))
          _  <- n3.receive()

          b4 <- n1.addBlock(deploys(2))
          b5 <- n2.addBlock(deploys(2))
          b6 <- n3.addBlock(deploys(2))
          _  <- n4.receive()

          b7  <- n3.addBlock(deploys(3))
          b8  <- n4.addBlock(deploys(5))
          b9  <- n3.addBlock(deploys(5))
          b10 <- n4.addBlock(deploys(4))

          _   <- n4.receive()
          dag <- n4.blockDagStorage.getRepresentation

          _ <- conflicts[Task](b2, b3, dag) shouldBeF false
          _ <- conflicts[Task](b4, b5, dag) shouldBeF true
          _ <- conflicts[Task](b6, b6, dag) shouldBeF false
          _ <- conflicts[Task](b6, b9, dag) shouldBeF false
          _ <- conflicts[Task](b7, b8, dag) shouldBeF false
          _ <- conflicts[Task](b7, b10, dag) shouldBeF false
          _ <- conflicts[Task](b9, b10, dag) shouldBeF true
        } yield ()
    }
  }

  it should "conflict if their deploys conflict in the deployLog" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      testConflict[Task] { deploy =>
        deploy.copy(
          deployLog = Seq(
            produce(ByteString.copyFromUtf8(channelsHash))
          )
        )
      } { deploy =>
        deploy.copy(deployLog = Seq(consume(ByteString.copyFromUtf8(channelsHash))))
      }
  }

  it should "conflict if their deploys conflict in the paymentLog" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      testConflict[Task] { deploy =>
        deploy.copy(paymentLog = Seq(produce(ByteString.copyFromUtf8(channelsHash))))
      } { deploy =>
        deploy.copy(paymentLog = Seq(consume(ByteString.copyFromUtf8(channelsHash))))
      }
  }

  private def testConflict[F[_]: BlockStore: IndexedBlockDagStorage: Time: Log: Monad](
      deployMod1: ProcessedDeploy => ProcessedDeploy
  )(
      deployMod2: ProcessedDeploy => ProcessedDeploy
  ): F[Unit] =
    for {
      genesis <- createGenesis[F]()
      deployA <- basicProcessedDeploy[F](1).map(deployMod1)
      a       <- createBlock[F](Seq(genesis.blockHash), genesis, deploys = Seq(deployA))
      deployB <- basicProcessedDeploy[F](2).map(deployMod2)
      b       <- createBlock[F](Seq(genesis.blockHash), genesis, deploys = Seq(deployB))
      dag     <- BlockDagStorage[F].getRepresentation

      _              <- conflicts[F](a, b, dag) shouldBeF true
      nonconflicting <- EstimatorHelper.chooseNonConflicting(Seq(a, b).map(_.blockHash), dag)
      result         = assert(nonconflicting == Seq(a))
    } yield ()

  private def produce(channelsHash: ByteString) =
    Event(
      Produce(
        ProduceEvent(
          channelsHash,
          hash = ByteString.copyFromUtf8("Asdfg753213fdsadfueias9fje35mv43")
        )
      )
    )

  private def consume(channelsHash: ByteString) =
    Event(
      Consume(
        ConsumeEvent(
          Seq(channelsHash),
          hash = ByteString.copyFromUtf8("Asdfg753213fdsadfueias9fje35mv43")
        )
      )
    )
}
