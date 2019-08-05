package coop.rchain.casper.api

import scala.collection.immutable.HashMap
import cats.effect.{Resource, Sync}
import coop.rchain.casper._
import coop.rchain.casper.engine._
import EngineCell._
import coop.rchain.casper.helper._
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.metrics.Span.TraceId
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.shared.{Cell, Log}
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}
import monix.execution.Scheduler.Implicits.global

// See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
class BlocksResponseAPITest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture {

  val v1                           = generateValidator("Validator One")
  val v2                           = generateValidator("Validator Two")
  val v3                           = generateValidator("Validator Three")
  val v1Bond                       = Bond(v1, 25)
  val v2Bond                       = Bond(v2, 20)
  val v3Bond                       = Bond(v3, 15)
  val bonds                        = Seq(v1Bond, v2Bond, v3Bond)
  implicit val spanEff: Span[Task] = Span.noop
  implicit val log                 = new Log.NOPLog[Task]()
  private val runtimeManagerResource: Resource[Task, RuntimeManager[Task]] =
    mkRuntimeManager("block-response-api-test")

  "showMainChain" should "return only blocks in the main chain" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
        for {
          genesis <- createGenesis[Task](bonds = bonds)
          b2 <- createBlock[Task](
                 Seq(genesis.blockHash),
                 genesis,
                 v2,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
               )
          b3 <- createBlock[Task](
                 Seq(genesis.blockHash),
                 genesis,
                 v1,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
               )
          b4 <- createBlock[Task](
                 Seq(b2.blockHash),
                 genesis,
                 v3,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash, v3 -> b2.blockHash)
               )
          b5 <- createBlock[Task](
                 Seq(b3.blockHash),
                 genesis,
                 v2,
                 bonds,
                 HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> genesis.blockHash)
               )
          b6 <- createBlock[Task](
                 Seq(b4.blockHash),
                 genesis,
                 v1,
                 bonds,
                 HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> b4.blockHash)
               )
          b7 <- createBlock[Task](
                 Seq(b5.blockHash),
                 genesis,
                 v3,
                 bonds,
                 HashMap(v1 -> b3.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
               )
          b8 <- createBlock[Task](
                 Seq(b6.blockHash),
                 genesis,
                 v2,
                 bonds,
                 HashMap(v1 -> b6.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
               )
          dag        <- blockDagStorage.getRepresentation
          metricsEff = new Metrics.MetricsNOP[Task]
          tips       <- Estimator.tips[Task](dag, genesis, traceId)(Sync[Task], log, metricsEff, spanEff)
          casperEffect <- NoOpsCasperEffect[Task](
                           HashMap.empty[BlockHash, BlockMessage],
                           tips
                         )
          logEff     = new LogStub[Task]
          engine     = new EngineWithCasper[Task](casperEffect)
          engineCell <- Cell.mvarCell[Task, Engine[Task]](engine)
          cliqueOracleEffect = SafetyOracle
            .cliqueOracle[Task](Sync[Task], logEff, metricsEff, spanEff)
          blocksResponse <- BlockAPI.showMainChain[Task](Int.MaxValue)(
                             Sync[Task],
                             engineCell,
                             logEff,
                             cliqueOracleEffect,
                             blockStore,
                             traceId
                           )
        } yield blocksResponse.length should be(5)
      }
  }

  "getBlocks" should "return all blocks" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      runtimeManagerResource.use { implicit runtimeManager =>
        for {
          genesis <- createGenesis[Task](bonds = bonds)
          b2 <- createBlock[Task](
                 Seq(genesis.blockHash),
                 genesis,
                 v2,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
               )
          b3 <- createBlock[Task](
                 Seq(genesis.blockHash),
                 genesis,
                 v1,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
               )
          b4 <- createBlock[Task](
                 Seq(b2.blockHash),
                 genesis,
                 v3,
                 bonds,
                 HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash, v3 -> b2.blockHash)
               )
          b5 <- createBlock[Task](
                 Seq(b3.blockHash),
                 genesis,
                 v2,
                 bonds,
                 HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> genesis.blockHash)
               )
          b6 <- createBlock[Task](
                 Seq(b4.blockHash),
                 genesis,
                 v1,
                 bonds,
                 HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> b4.blockHash)
               )
          b7 <- createBlock[Task](
                 Seq(b5.blockHash),
                 genesis,
                 v3,
                 bonds,
                 HashMap(v1 -> b3.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
               )
          b8 <- createBlock[Task](
                 Seq(b6.blockHash),
                 genesis,
                 v2,
                 bonds,
                 HashMap(v1 -> b6.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
               )
          dag        <- blockDagStorage.getRepresentation
          metricsEff = new Metrics.MetricsNOP[Task]
          tips       <- Estimator.tips[Task](dag, genesis, traceId)(Sync[Task], log, metricsEff, spanEff)
          casperEffect <- NoOpsCasperEffect[Task](
                           HashMap.empty[BlockHash, BlockMessage],
                           tips
                         )
          engine     = new EngineWithCasper[Task](casperEffect)
          engineCell <- Cell.mvarCell[Task, Engine[Task]](engine)
          logEff     = new LogStub[Task]
          cliqueOracleEffect = SafetyOracle
            .cliqueOracle[Task](Sync[Task], logEff, metricsEff, spanEff)
          blocksResponse <- BlockAPI.getBlocks[Task](Some(Int.MaxValue))(
                             Sync[Task],
                             engineCell,
                             logEff,
                             cliqueOracleEffect,
                             blockStore,
                             traceId
                           )
        } yield blocksResponse.right.get.length should be(8) // TODO: Switch to 4 when we implement block height correctly
      }
  }

  it should "return until depth" in withStorage { implicit blockStore => implicit blockDagStorage =>
    runtimeManagerResource.use { implicit runtimeManager =>
      for {
        genesis <- createGenesis[Task](bonds = bonds)
        b2 <- createBlock[Task](
               Seq(genesis.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
             )
        b3 <- createBlock[Task](
               Seq(genesis.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> genesis.blockHash, v3 -> genesis.blockHash)
             )
        b4 <- createBlock[Task](
               Seq(b2.blockHash),
               genesis,
               v3,
               bonds,
               HashMap(v1 -> genesis.blockHash, v2 -> b2.blockHash, v3 -> b2.blockHash)
             )
        b5 <- createBlock[Task](
               Seq(b3.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> genesis.blockHash)
             )
        b6 <- createBlock[Task](
               Seq(b4.blockHash),
               genesis,
               v1,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b2.blockHash, v3 -> b4.blockHash)
             )
        b7 <- createBlock[Task](
               Seq(b5.blockHash),
               genesis,
               v3,
               bonds,
               HashMap(v1 -> b3.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
             )
        b8 <- createBlock[Task](
               Seq(b6.blockHash),
               genesis,
               v2,
               bonds,
               HashMap(v1 -> b6.blockHash, v2 -> b5.blockHash, v3 -> b4.blockHash)
             )
        dag        <- blockDagStorage.getRepresentation
        metricsEff = new Metrics.MetricsNOP[Task]
        tips       <- Estimator.tips[Task](dag, genesis, traceId)(Sync[Task], log, metricsEff, spanEff)
        casperEffect <- NoOpsCasperEffect[Task](
                         HashMap.empty[BlockHash, BlockMessage],
                         tips
                       )
        logEff     = new LogStub[Task]
        engine     = new EngineWithCasper[Task](casperEffect)
        engineCell <- Cell.mvarCell[Task, Engine[Task]](engine)
        cliqueOracleEffect = SafetyOracle
          .cliqueOracle[Task](Sync[Task], logEff, metricsEff, spanEff)
        blocksResponse <- BlockAPI.getBlocks[Task](Some(2))(
                           Sync[Task],
                           engineCell,
                           logEff,
                           cliqueOracleEffect,
                           blockStore,
                           traceId
                         )
      } yield blocksResponse.right.get.length should be(2) // TODO: Switch to 3 when we implement block height correctly
    }
  }
}
