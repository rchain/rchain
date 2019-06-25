package coop.rchain.casper

import cats.Monad
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagStorage, BlockStore, IndexedBlockDagStorage}
import coop.rchain.casper.EstimatorHelper.conflicts
import coop.rchain.casper.helper.BlockGenerator.{
  computeBlockCheckpoint,
  injectPostStateHash,
  updateChainWithBlockStateUpdate
}
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.protocol.Event.EventInstance.Produce
import coop.rchain.casper.protocol.{Event, ProcessedDeploy, ProduceEvent}
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy.basicProcessedDeploy
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
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
  "Blocks" should "conflict if they use the same deploys in different histories" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      mkRuntimeManager("casper-util-test").use { runtimeManager =>
        for {

          deploys <- (0 until 6).toList.traverse(i => basicProcessedDeploy[Task](i))
          genesis <- createGenesis[Task]()
          b2      <- createBlock[Task](Seq(genesis.blockHash), genesis, deploys = Seq(deploys(0)))
          b3      <- createBlock[Task](Seq(genesis.blockHash), genesis, deploys = Seq(deploys(1)))
          b4      <- createBlock[Task](Seq(b2.blockHash), genesis, deploys = Seq(deploys(2)))
          b5      <- createBlock[Task](Seq(b3.blockHash), genesis, deploys = Seq(deploys(2)))
          b6 <- createBlock[Task](
                 Seq(b2.blockHash, b3.blockHash),
                 genesis,
                 deploys = Seq(deploys(2))
               )
          b7  <- createBlock[Task](Seq(b6.blockHash), genesis, deploys = Seq(deploys(3)))
          b8  <- createBlock[Task](Seq(b6.blockHash), genesis, deploys = Seq(deploys(5)))
          b9  <- createBlock[Task](Seq(b7.blockHash), genesis, deploys = Seq(deploys(5)))
          b10 <- createBlock[Task](Seq(b8.blockHash), genesis, deploys = Seq(deploys(4)))

          dag <- blockDagStorage.getRepresentation

          computeBlockCheckpointResult <- computeBlockCheckpoint(
                                           genesis,
                                           genesis,
                                           dag,
                                           runtimeManager
                                         )
          (postGenStateHash, postGenProcessedDeploys) = computeBlockCheckpointResult
          _ <- injectPostStateHash[Task](
                0,
                genesis,
                genesis,
                postGenStateHash,
                postGenProcessedDeploys
              )
          _ <- updateChainWithBlockStateUpdate[Task](1, genesis, runtimeManager)
          _ <- updateChainWithBlockStateUpdate[Task](2, genesis, runtimeManager)
          _ <- updateChainWithBlockStateUpdate[Task](3, genesis, runtimeManager)
          _ <- updateChainWithBlockStateUpdate[Task](4, genesis, runtimeManager)
          _ <- updateChainWithBlockStateUpdate[Task](5, genesis, runtimeManager)
          _ <- updateChainWithBlockStateUpdate[Task](6, genesis, runtimeManager)
          _ <- updateChainWithBlockStateUpdate[Task](7, genesis, runtimeManager)
          _ <- updateChainWithBlockStateUpdate[Task](8, genesis, runtimeManager)
          _ <- updateChainWithBlockStateUpdate[Task](9, genesis, runtimeManager)

          _      <- conflicts[Task](b2, b3, dag) shouldBeF false
          _      <- conflicts[Task](b4, b5, dag) shouldBeF true
          _      <- conflicts[Task](b6, b6, dag) shouldBeF false
          _      <- conflicts[Task](b6, b9, dag) shouldBeF false
          _      <- conflicts[Task](b7, b8, dag) shouldBeF false
          _      <- conflicts[Task](b7, b10, dag) shouldBeF false
          result <- conflicts[Task](b9, b10, dag) shouldBeF true
        } yield result
      }
  }

  it should "conflict if their deploys contain same channel in deployLog" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      testConflict[Task] { deploy =>
        deploy.copy(deployLog = Seq(produce(ByteString.copyFromUtf8("A"))))
      }
  }

  it should "conflict if their deploys contain same channel in paymentLog" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      testConflict[Task] { deploy =>
        deploy.copy(paymentLog = Seq(produce(ByteString.copyFromUtf8("A"))))
      }
  }

  private def testConflict[F[_]: BlockStore: IndexedBlockDagStorage: Time: Log: Monad](
      deployMod: ProcessedDeploy => ProcessedDeploy
  ): F[Unit] =
    for {
      genesis <- createGenesis[F]()
      deployA <- basicProcessedDeploy[F](1).map(deployMod)
      a       <- createBlock[F](Seq(genesis.blockHash), genesis, deploys = Seq(deployA))
      deployB <- basicProcessedDeploy[F](2).map(deployMod)
      b       <- createBlock[F](Seq(genesis.blockHash), genesis, deploys = Seq(deployB))
      dag     <- BlockDagStorage[F].getRepresentation

      _              <- conflicts[F](a, b, dag) shouldBeF true
      nonconflicting <- EstimatorHelper.chooseNonConflicting(Seq(a, b).map(_.blockHash), dag)
      result         = assert(nonconflicting == Seq(a))
    } yield ()

  private def produce(channelsHash: ByteString) =
    Event(
      Produce(
        ProduceEvent(channelsHash)
      )
    )
}
