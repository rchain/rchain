package coop.rchain.casper.batch2

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.IndexedBlockDagStorage
import coop.rchain.casper.syntax._
import coop.rchain.casper.finality.Finalizer
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.syntax._
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances.LogStub
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap

class FinalizerTest extends FlatSpec with Matchers with BlockGenerator with BlockDagStorageFixture {
  behavior of "Finalizer"

  implicit val logEff               = new LogStub[Task]
  implicit val metricsEff           = new Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task] = NoopSpan[Task]()

  def createBlock(bonds: Seq[Bond])(genesis: BlockMessage)(creator: ByteString)(
      parents: Seq[BlockMessage],
      justifications: Map[Validator, BlockMessage]
  )(implicit store: BlockStore[Task], dagStore: IndexedBlockDagStorage[Task]): Task[BlockMessage] =
    createBlock[Task](
      parents.map(_.blockHash),
      genesis,
      creator = creator,
      bonds = bonds,
      justifications = justifications.map { case (v, bm) => (v, bm.blockHash) }
    )

  /**
    *   *  *            b8 b9
    *   *               b7         <- should not be LFB
    *   *  *  *  *  *   b2 b3 b4 b5 b6
    *   *               b1         <- should be LFB
    *   v1 v2 v3 v4 v5
    */
  it should "not advance finalization if no new LFB found, advance otherwise, invoke all effects" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val validators @ Seq(v1, v2, v3, v4, v5) =
        Seq(1, 2, 3, 4, 5).map(i => generateValidator(s"Validator ${i}"))
      val bonds               = validators.map(Bond(_, 3))
      implicit val concurrent = Concurrent[Task]

      var lfbStore         = ByteString.EMPTY
      var lfbEffectInvoked = false

      for {
        genesis  <- createGenesis[Task](bonds = bonds)
        creator1 = createBlock(bonds)(genesis)(v1) _
        creator2 = createBlock(bonds)(genesis)(v2) _
        creator3 = createBlock(bonds)(genesis)(v3) _
        creator4 = createBlock(bonds)(genesis)(v4) _
        creator5 = createBlock(bonds)(genesis)(v5) _
        genesisJustification = HashMap(
          v1 -> genesis,
          v2 -> genesis,
          v3 -> genesis,
          v4 -> genesis,
          v5 -> genesis
        )
        finalisedStore <- Ref.of[Task, Set[BlockHash]](Set.empty)

        b1 <- creator1(Seq(genesis), genesisJustification)

        /** add children on all blocks - this should finalize parent */
        b2 <- creator1(Seq(b1), genesisJustification)
        b3 <- creator2(Seq(b1), genesisJustification)
        b4 <- creator3(Seq(b1), genesisJustification)
        b5 <- creator4(Seq(b1), genesisJustification)
        b6 <- creator5(Seq(b1), genesisJustification)

        dag <- blockDagStorage.getRepresentation
        lms <- dag.latestMessages.map(_.toSeq.map { case (v, m) => (v, m.blockHash) })
        lfb <- Finalizer.run[Task](
                dag,
                faultToleranceThreshold = -1,
                currLFBHeight = 0L,
                m => (lfbStore = m).pure[Task]
              )
        // check output
        _ = lfb.get shouldBe b1.blockHash
        // check if new LFB effect is invoked
        _ = lfbStore shouldBe b1.blockHash

        finalizedHeight <- dag.lookupUnsafe(lfb.get).map(_.blockNum)

        /** next layer */
        b7 <- creator1(
               Seq(b2, b3, b4, b5, b6),
               HashMap(v1 -> b2, v2 -> b3, v3 -> b4, v4 -> b4, v5 -> b5)
             )

        /** add 2 children, this is not sufficient for finalization to advance */
        _ <- creator1(Seq(b7), HashMap(v1 -> b7, v2 -> b3, v3 -> b4, v4 -> b5, v5 -> b6))
        _ <- creator2(Seq(b7), HashMap(v1 -> b7, v2 -> b3, v3 -> b4, v4 -> b5, v5 -> b6))

        dag <- blockDagStorage.getRepresentation
        lfb <- Finalizer.run[Task](
                dag,
                faultToleranceThreshold = -1,
                currLFBHeight = finalizedHeight,
                m => (lfbEffectInvoked = true).pure[Task]
              )
        // check output
        _ = lfb shouldBe None
        // check if new LFB effect is invoked
        _ = lfbEffectInvoked shouldBe false

        /** add more 3 children - finalization should advance*/
        _ <- creator3(Seq(b7), HashMap(v1 -> b7, v2 -> b3, v3 -> b4, v4 -> b5, v5 -> b6))
        _ <- creator4(Seq(b7), HashMap(v1 -> b7, v2 -> b3, v3 -> b4, v4 -> b5, v5 -> b6))
        _ <- creator5(Seq(b7), HashMap(v1 -> b7, v2 -> b3, v3 -> b4, v4 -> b5, v5 -> b6))

        dag <- blockDagStorage.getRepresentation
        lfb <- Finalizer.run[Task](
                dag,
                faultToleranceThreshold = -1,
                currLFBHeight = 0L,
                m => (lfbStore = m).pure[Task] <* finalisedStore.update(v => v + m)
              )
        // check output
        _ = lfb shouldBe Some(b7.blockHash)
        // check if new LFB effect is invoked
        _ = lfbStore shouldBe b7.blockHash
      } yield ()
  }

}
