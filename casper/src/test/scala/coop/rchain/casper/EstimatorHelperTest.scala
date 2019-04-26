package coop.rchain.casper

import cats.implicits._
import coop.rchain.casper.EstimatorHelper.conflicts
import coop.rchain.casper.helper.BlockGenerator.{
  computeBlockCheckpoint,
  injectPostStateHash,
  updateChainWithBlockStateUpdate
}
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy.basicProcessedDeploy
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

class EstimatorHelperTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture {

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
    implicit val log: Log[Task] = new Log.NOPLog[Task]
    implicit val timeEff        = new LogicalTime[Task]

    implicit blockStore => implicit blockDagStorage =>
      for {
        deploys <- (0 until 6).toList.traverse(i => basicProcessedDeploy[Task](i))
        genesis <- createGenesis[Task]()
        b2      <- createBlock[Task](Seq(genesis.blockHash), genesis, deploys = Seq(deploys(0)))
        b3      <- createBlock[Task](Seq(genesis.blockHash), genesis, deploys = Seq(deploys(1)))
        b4      <- createBlock[Task](Seq(b2.blockHash), genesis, deploys = Seq(deploys(2)))
        b5      <- createBlock[Task](Seq(b3.blockHash), genesis, deploys = Seq(deploys(2)))
        b6      <- createBlock[Task](Seq(b2.blockHash, b3.blockHash), genesis, deploys = Seq(deploys(2)))
        b7      <- createBlock[Task](Seq(b6.blockHash), genesis, deploys = Seq(deploys(3)))
        b8      <- createBlock[Task](Seq(b6.blockHash), genesis, deploys = Seq(deploys(5)))
        b9      <- createBlock[Task](Seq(b7.blockHash), genesis, deploys = Seq(deploys(5)))
        b10     <- createBlock[Task](Seq(b8.blockHash), genesis, deploys = Seq(deploys(4)))

        dag <- blockDagStorage.getRepresentation
        result <- mkRuntimeManager("casper-util-test").use { runtimeManager =>
                   for {
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
      } yield result
  }
}
