package coop.rchain.casper

import cats.effect.Sync
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import coop.rchain.catscontrib.effect.implicits.syncId
import coop.rchain.p2p.EffectsTestInstances.LogStub
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Random

class ManyValidatorsTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockStoreTestFixture {
  "Show blocks" should "be processed quickly for a node with 300 validators" in {
    import monix.execution.Scheduler.Implicits.global

    val initState = IndexedBlockDag.empty.withOffset(1L)
    val bonds     = Seq.fill(300)(ByteString.copyFromUtf8(Random.nextString(10))).map(Bond(_, 10))
    val v1        = bonds(0).validator

    val createChain =
      for {
        genesis <- createBlock[StateWithChain](Seq(), ByteString.EMPTY, bonds)
        b <- createBlock[StateWithChain](Seq(genesis.blockHash), v1, bonds, bonds.map {
              case Bond(validator, _) => validator -> genesis.blockHash
            }.toMap)
      } yield b

    val initialChain: IndexedBlockDag = createChain.runS(initState).runSyncUnsafe(1.second)
    val genesis                       = initialChain.idToBlocks(1)
    val n1                            = initialChain.idToBlocks(2)
    val otherLatestMessages           = bonds.map { case Bond(validator, _) => validator -> n1 }
    val chain =
      initialChain.copy(
        dag = initialChain.dag.copy(
          latestMessages = initialChain.latestMessages ++ otherLatestMessages
        )
      )

    implicit val casperEffect: MultiParentCasper[Task] =
      NoOpsCasperEffect[Task](
        HashMap.empty[BlockHash, BlockMessage],
        Estimator.tips[Task](chain, genesis.blockHash).runSyncUnsafe(1.second),
        chain
      )(Sync[Task], blockStore).runSyncUnsafe(1.second)
    implicit val logEff = new LogStub[Task]
    implicit val casperRef = {
      val tmp = MultiParentCasperRef.of[Task].runSyncUnsafe(1.second)
      tmp.set(casperEffect)
      tmp
    }
    implicit val turanOracleEffect: SafetyOracle[Task] = SafetyOracle.turanOracle[Task]
    Await.result(Future(BlockAPI.showBlocks[Task](Int.MaxValue).runSyncUnsafe(1.second)), 1 minute)
  }
}
