package coop.rchain.casper.util

import ProtoUtil._
import com.google.protobuf.ByteString
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.catscontrib._
import cats._
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator, BlockStoreFixture}
import cats.data._
import cats.effect.Bracket
import cats.implicits._
import cats.mtl.MonadState
import cats.mtl.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.helper.{BlockGenerator, BlockStoreFixture, IndexedBlockDag}
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.util.rholang.{InterpreterUtil, ProcessedDeployUtil, RuntimeManager}
import coop.rchain.casper.util.rholang.RuntimeManager.StateHash
import coop.rchain.shared.Time
import monix.eval.Task
import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.{HashMap, HashSet}

class CasperUtilTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockStoreFixture
    with BlockDagStorageFixture {
  "isInMainChain" should "classify appropriately" in withStore { implicit blockStore =>
    withIndexedBlockDagStorage { implicit blockDagStorage =>
      val genesis = createBlock[Id](Seq())
      val b2      = createBlock[Id](Seq(genesis.blockHash))
      val b3      = createBlock[Id](Seq(b2.blockHash))

      val dag = blockDagStorage.getRepresentation

      isInMainChain(dag, genesis.blockHash, b3.blockHash) should be(true)
      isInMainChain(dag, b2.blockHash, b3.blockHash) should be(true)
      isInMainChain(dag, b3.blockHash, b2.blockHash) should be(false)
      isInMainChain(dag, b3.blockHash, genesis.blockHash) should be(false)
    }
  }

  "isInMainChain" should "classify diamond DAGs appropriately" in withStore { implicit blockStore =>
    withIndexedBlockDagStorage { implicit blockDagStorage =>
      val genesis = createBlock[Id](Seq())
      val b2      = createBlock[Id](Seq(genesis.blockHash))
      val b3      = createBlock[Id](Seq(genesis.blockHash))
      val b4      = createBlock[Id](Seq(b2.blockHash, b3.blockHash))

      val dag = blockDagStorage.getRepresentation

      isInMainChain(dag, genesis.blockHash, b2.blockHash) should be(true)
      isInMainChain(dag, genesis.blockHash, b3.blockHash) should be(true)
      isInMainChain(dag, genesis.blockHash, b4.blockHash) should be(true)
      isInMainChain(dag, b2.blockHash, b4.blockHash) should be(true)
      isInMainChain(dag, b3.blockHash, b4.blockHash) should be(false)
    }
  }

  // See https://docs.google.com/presentation/d/1znz01SF1ljriPzbMoFV0J127ryPglUYLFyhvsb-ftQk/edit?usp=sharing slide 29 for diagram
  "isInMainChain" should "classify complicated chains appropriately" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        val v1 = ByteString.copyFromUtf8("Validator One")
        val v2 = ByteString.copyFromUtf8("Validator Two")

        val genesis = createBlock[Id](Seq(), ByteString.EMPTY)
        val b2      = createBlock[Id](Seq(genesis.blockHash), v2)
        val b3      = createBlock[Id](Seq(genesis.blockHash), v1)
        val b4      = createBlock[Id](Seq(b2.blockHash), v2)
        val b5      = createBlock[Id](Seq(b2.blockHash), v1)
        val b6      = createBlock[Id](Seq(b4.blockHash), v2)
        val b7      = createBlock[Id](Seq(b4.blockHash), v1)
        val b8      = createBlock[Id](Seq(b7.blockHash), v1)

        val dag = blockDagStorage.getRepresentation

        isInMainChain(dag, genesis.blockHash, b2.blockHash) should be(true)
        isInMainChain(dag, b2.blockHash, b3.blockHash) should be(false)
        isInMainChain(dag, b3.blockHash, b4.blockHash) should be(false)
        isInMainChain(dag, b4.blockHash, b5.blockHash) should be(false)
        isInMainChain(dag, b5.blockHash, b6.blockHash) should be(false)
        isInMainChain(dag, b6.blockHash, b7.blockHash) should be(false)
        isInMainChain(dag, b7.blockHash, b8.blockHash) should be(true)
        isInMainChain(dag, b2.blockHash, b6.blockHash) should be(true)
        isInMainChain(dag, b2.blockHash, b8.blockHash) should be(true)
        isInMainChain(dag, b4.blockHash, b2.blockHash) should be(false)
      }
  }

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
  "Blocks" should "conflict if they use the same deploys in different histories" in withStore {
    implicit blockStore =>
      withIndexedBlockDagStorage { implicit blockDagStorage =>
        val deploys = (0 until 6).map(basicProcessedDeploy[Id])

        val genesis = createBlock[Id](Seq())
        val b2      = createBlock[Id](Seq(genesis.blockHash), deploys = Seq(deploys(0)))
        val b3      = createBlock[Id](Seq(genesis.blockHash), deploys = Seq(deploys(1)))
        val b4      = createBlock[Id](Seq(b2.blockHash), deploys = Seq(deploys(2)))
        val b5      = createBlock[Id](Seq(b3.blockHash), deploys = Seq(deploys(2)))
        val b6      = createBlock[Id](Seq(b2.blockHash, b3.blockHash), deploys = Seq(deploys(2)))
        val b7      = createBlock[Id](Seq(b6.blockHash), deploys = Seq(deploys(3)))
        val b8      = createBlock[Id](Seq(b6.blockHash), deploys = Seq(deploys(5)))
        val b9      = createBlock[Id](Seq(b7.blockHash), deploys = Seq(deploys(5)))
        val b10     = createBlock[Id](Seq(b8.blockHash), deploys = Seq(deploys(4)))

        val dag = blockDagStorage.getRepresentation
        mkRuntimeManager("casper-util-test")
          .use { runtimeManager =>

            conflicts[Id](b2, b3, genesis, dag) should be(false)
            conflicts[Id](b4, b5, genesis, dag) should be(true)
            conflicts[Id](b6, b6, genesis, dag) should be(false)
            conflicts[Id](b6, b9, genesis, dag) should be(false)
            conflicts[Id](b7, b8, genesis, dag) should be(false)
            conflicts[Id](b7, b10, genesis, dag) should be(false)
            conflicts[Id](b9, b10, genesis, dag) should be(true)
          }
      }
  }
}
