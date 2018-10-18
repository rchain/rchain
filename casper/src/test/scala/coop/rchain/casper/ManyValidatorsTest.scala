package coop.rchain.casper

import cats.Id
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.BlockHash
import coop.rchain.casper.api.BlockAPI
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper._
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import coop.rchain.catscontrib.effect.implicits.syncId
import coop.rchain.p2p.EffectsTestInstances.LogStub
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class ManyValidatorsTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockStoreTestFixture {
  "Show blocks" should "be processed quickly for a node with 300 validators" in {
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

    val initialChain: IndexedBlockDag = createChain.runS(initState)
    val genesis                       = initialChain.idToBlocks(1)
    val n1                            = initialChain.idToBlocks(2)
    val otherLatestMessages           = bonds.map { case Bond(validator, _) => validator -> n1 }
    val chain =
      initialChain.copy(
        dag = initialChain.dag.copy(
          latestMessages = initialChain.latestMessages ++ otherLatestMessages
        )
      )

    implicit val casperEffect: MultiParentCasper[Id] =
      NoOpsCasperEffect[Id](
        HashMap.empty[BlockHash, BlockMessage],
        Estimator.tips[Id](chain, genesis.blockHash),
        chain
      )(syncId, blockStore)
    implicit val logEff = new LogStub[Id]
    implicit val casperRef = {
      val tmp = MultiParentCasperRef.of[Id]
      tmp.set(casperEffect)
      tmp
    }
    implicit val turanOracleEffect: SafetyOracle[Id] = SafetyOracle.turanOracle[Id]
    Await.result(Future(BlockAPI.showBlocks[Id](Int.MaxValue)), 1 minute)
  }
}
