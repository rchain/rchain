package coop.rchain.casper.api

import scala.concurrent.duration._
import cats.Monad
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.{Deferred, Semaphore}
import cats.implicits._
import coop.rchain.casper.engine._
import EngineCell._
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper.{LastFinalizedHeightConstraintChecker, SynchronyConstraintChecker, _}
import coop.rchain.casper.api.BlockAPI.ApiErr
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.protocol._
import coop.rchain.casper.util._
import coop.rchain.casper.util.ConstructDeploy.basicDeployData
import coop.rchain.casper.util.rholang._
import coop.rchain.casper.batch2.EngineWithCasper
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.signatures.{Secp256k1, Signed}
import coop.rchain.metrics._
import coop.rchain.metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared._
import coop.rchain.shared.scalatestcontrib._
import fs2.concurrent.Queue
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{Engine => _, _}

class CreateBlockAPITest extends FlatSpec with Matchers with EitherValues {

  import GenesisBuilder._
  import TestNode.Effect

  def createBlock(
      proposerQueue: Queue[Task, (Casper[Task], Deferred[Task, Option[Int]])]
  )(engineCell: Cell[Task, Engine[Task]])(
      implicit log: Log[Task]
  ): Task[Either[String, String]] =
    BlockAPI.createBlock[Task](proposerQueue)(
      Concurrent[Task],
      engineCell,
      log
    )

  val validatorKeyPairs = (1 to 5).map(_ => Secp256k1.newKeyPair)
  val genesisParameters = buildGenesisParameters(
    validatorKeyPairs,
    validatorKeyPairs.map(_._2).zip(List.fill(5)(10L)).toMap
  )
  val genesis = buildGenesis(genesisParameters)

  implicit val scheduler = Scheduler.fixedPool("three-threads", 3)

  // This tests do not make much sense after introducing Proposer

  //  "createBlock" should "not allow simultaneous calls" in effectTest {
  //    implicit val metricsEff: Metrics[Effect] = new metrics.Metrics.MetricsNOP[Effect]
  //    implicit val logEff                      = new LogStub[Effect]
  //    implicit val spanEff                     = NoopSpan[Effect]
  //    implicit val time = new Time[Task] {
  //      private val timer                               = Task.timer
  //      def currentMillis: Task[Long]                   = timer.clock.realTime(MILLISECONDS)
  //      def nanoTime: Task[Long]                        = timer.clock.monotonic(NANOSECONDS)
  //      def sleep(duration: FiniteDuration): Task[Unit] = timer.sleep(duration)
  //    }
  //    TestNode.standaloneEff(genesis).use { node =>
  //      val casper = new SleepingMultiParentCasperImpl[Effect](node.casperEff)
  //      val deploys = List(
  //        "@0!(0) | for(_ <- @0){ @1!(1) }",
  //        "for(_ <- @1){ @2!(2) }"
  //      ).map(ConstructDeploy.sourceDeploy(_, timestamp = System.currentTimeMillis()))
  //
  //      def createBlock(deploy: Signed[DeployData], blockApiLock: Semaphore[Effect])(
  //          implicit engineCell: EngineCell[Effect],
  //          sync: SynchronyConstraintChecker[Effect],
  //          lfhcc: LastFinalizedHeightConstraintChecker[Effect]
  //      ): Effect[ApiErr[String]] =
  //        for {
  //          _ <- BlockAPI.deploy[Effect](deploy)
  //          r <- BlockAPI.createBlock[Effect]
  //        } yield r
  //
  //      def testProgram(blockApiLock: Semaphore[Effect])(
  //          implicit engineCell: EngineCell[Effect],
  //          sync: SynchronyConstraintChecker[Effect],
  //          lfhcc: LastFinalizedHeightConstraintChecker[Effect]
  //      ): Effect[
  //        (
  //            ApiErr[String],
  //            ApiErr[String],
  //            ApiErr[String]
  //        )
  //      ] =
  //        for {
  //          t1 <- createBlock(deploys.head, blockApiLock).start
  //          _  <- Time[Task].sleep(2.second)
  //          t2 <- createBlock(deploys.last, blockApiLock).start //should fail because other not done
  //          t3 <- createBlock(deploys.last, blockApiLock).start //should fail because other not done
  //          r1 <- t1.join.attempt
  //          r2 <- t2.join.attempt
  //          r3 <- t3.join.attempt
  //        } yield (r1.right.value, r2.right.value, r3.right.value)
  //
  //      implicit val blockStore                 = node.blockStore
  //      implicit val lastFinalizedStorage       = node.lastFinalizedStorage
  //      implicit val synchronyConstraintChecker = SynchronyConstraintChecker[Effect]
  //      implicit val lastFinalizedHeightConstraintChecker =
  //        LastFinalizedHeightConstraintChecker[Effect]
  //      val (response1, response2, response3) = (for {
  //        engine       <- new EngineWithCasper[Task](casper).pure[Task]
  //        engineCell   <- Cell.mvarCell[Task, Engine[Task]](engine)
  //        blockApiLock <- Semaphore[Effect](1)
  //
  //        result <- testProgram(blockApiLock)(
  //                   engineCell,
  //                   synchronyConstraintChecker,
  //                   lastFinalizedHeightConstraintChecker
  //                 )
  //      } yield result).unsafeRunSync
  //
  //      response1 shouldBe a[Right[_, String]]
  //      response2 shouldBe a[Left[_, String]]
  //      response3 shouldBe a[Left[_, String]]
  //      response2.left.value shouldBe "Error: There is another propose in progress."
  //      response3.left.value shouldBe "Error: There is another propose in progress."
  //
  //      ().pure[Effect]
  //    }
  //  }
  //
  //  val syncConstraintThreshold = 1d / 3d
  //
  //  it should "not allow proposals without enough new blocks from other validators" in effectTest {
  //    TestNode
  //      .networkEff(genesis, networkSize = 5, synchronyConstraintThreshold = syncConstraintThreshold)
  //      .use {
  //        case nodes @ n1 +: n2 +: _ +: _ +: _ +: Seq() =>
  //          import n1.{logEff, metricEff, span, timeEff}
  //          val engine                        = new EngineWithCasper[Task](n1.casperEff)
  //          implicit val blockStore           = n1.blockStore
  //          implicit val lastFinalizedStorage = n1.lastFinalizedStorage
  //          implicit val synchronyConstraintChecker =
  //            SynchronyConstraintChecker[Effect](syncConstraintThreshold)
  //          implicit val lastFinalizedHeightConstraintChecker =
  //            LastFinalizedHeightConstraintChecker[Effect](Long.MaxValue)
  //          for {
  //            deploys      <- (0 until 3).toList.traverse(i => basicDeployData[Task](i))
  //            engineCell   <- Cell.mvarCell[Task, Engine[Task]](engine)
  //            blockApiLock <- Semaphore[Effect](1)
  //
  //            b1 <- n1.publishBlock(deploys(0))(nodes: _*)
  //            b2 <- n2.publishBlock(deploys(1))(nodes: _*)
  //            _  <- n1.syncWith(n2)
  //
  //            _        <- n1.casperEff.addDeploy(deploys(2))
  //            b3Status <- createBlock(blockApiLock)(engineCell)
  //
  //            _ = b3Status.left.value should include(
  //              "Must wait for more blocks from other validators"
  //            )
  //          } yield ()
  //      }
  //  }
  //
  //  it should "allow proposals with enough new blocks from other validators" in effectTest {
  //    TestNode
  //      .networkEff(genesis, networkSize = 5, synchronyConstraintThreshold = syncConstraintThreshold)
  //      .use {
  //        case nodes @ n1 +: n2 +: n3 +: _ +: _ +: Seq() =>
  //          import n1.{logEff, metricEff, span, timeEff}
  //          val engine                        = new EngineWithCasper[Task](n1.casperEff)
  //          implicit val blockStore           = n1.blockStore
  //          implicit val lastFinalizedStorage = n1.lastFinalizedStorage
  //          implicit val synchronyConstraintChecker =
  //            SynchronyConstraintChecker[Effect](syncConstraintThreshold)
  //          implicit val lastFinalizedHeightConstraintChecker =
  //            LastFinalizedHeightConstraintChecker[Effect](Long.MaxValue)
  //          for {
  //            deploys      <- (0 until 4).toList.traverse(i => basicDeployData[Task](i))
  //            engineCell   <- Cell.mvarCell[Task, Engine[Task]](engine)
  //            blockApiLock <- Semaphore[Effect](1)
  //
  //            b1 <- n1.publishBlock(deploys(0))(nodes: _*)
  //            b2 <- n2.publishBlock(deploys(1))(nodes: _*)
  //            b3 <- n3.publishBlock(deploys(2))(nodes: _*)
  //            _  <- n1.syncWith(n2, n3)
  //
  //            _        <- n1.casperEff.addDeploy(deploys(3))
  //            b4Status <- createBlock(blockApiLock)(engineCell)
  //
  //            _ = b4Status shouldBe a[Right[_, String]]
  //          } yield ()
  //      }
  //  }
  //
  //  it should "check for new deploys before checking synchrony constraint" ignore effectTest {
  //    TestNode
  //      .networkEff(genesis, networkSize = 5, synchronyConstraintThreshold = syncConstraintThreshold)
  //      .use {
  //        case nodes @ n1 +: n2 +: _ +: _ +: _ +: Seq() =>
  //          import n1.{logEff, metricEff, span, timeEff}
  //          val engine                        = new EngineWithCasper[Task](n1.casperEff)
  //          implicit val blockStore           = n1.blockStore
  //          implicit val lastFinalizedStorage = n1.lastFinalizedStorage
  //          implicit val synchronyConstraintChecker =
  //            SynchronyConstraintChecker[Effect](syncConstraintThreshold)
  //          implicit val lastFinalizedHeightConstraintChecker =
  //            LastFinalizedHeightConstraintChecker[Effect](Long.MaxValue)
  //          for {
  //            deploys      <- (0 until 3).toList.traverse(i => basicDeployData[Task](i))
  //            engineCell   <- Cell.mvarCell[Task, Engine[Task]](engine)
  //            blockApiLock <- Semaphore[Effect](1)
  //
  //            b1 <- n1.publishBlock(deploys(0))(nodes: _*)
  //            b2 <- n2.publishBlock(deploys(1))(nodes: _*)
  //
  //            b3Status <- createBlock(blockApiLock)(engineCell)
  //
  //            _ = b3Status.left.value should include("NoNewDeploys")
  //          } yield ()
  //      }
  //  }
  //}

//  private class SleepingMultiParentCasperImpl[F[_]: Monad: Time](underlying: MultiParentCasper[F])
//      extends MultiParentCasper[F] {
//
//    def deploy(d: Signed[DeployData]): F[Either[DeployError, DeployId]] = underlying.deploy(d)
//    def estimator(dag: BlockDagRepresentation[F]): F[IndexedSeq[BlockHash]] =
//      underlying.estimator(dag)
//    def blockDag: F[BlockDagRepresentation[F]] = underlying.blockDag
//    def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] =
//      underlying.normalizedInitialFault(weights)
//    def lastFinalizedBlock: F[BlockMessage]                  = underlying.lastFinalizedBlock
//    def getRuntimeManager: F[RuntimeManager[F]]              = underlying.getRuntimeManager
//    def fetchDependencies: F[Unit]                           = underlying.fetchDependencies
//    def getGenesis: F[BlockMessage]                          = underlying.getGenesis
//    def getValidator: F[Option[ValidatorIdentity]]           = underlying.getValidator
//    override def contains(hash: BlockHash): F[Boolean]       = underlying.contains(hash)
//    override def dagContains(hash: BlockHash): F[Boolean]    = underlying.dagContains(hash)
//    override def bufferContains(hash: BlockHash): F[Boolean] = underlying.bufferContains(hash)
//    override def getVersion: F[Long]                         = underlying.getVersion
//
//    override def getSnapshot: F[CasperSnapshot[F]] = ???
//    override def validate(
//        b: BlockMessage,
//        dag: BlockDagRepresentation[F]
//    ): F[Either[BlockError, ValidBlock]]                                             = ???
//    override def handleValidBlock(block: BlockMessage): F[BlockDagRepresentation[F]] = ???
//    override def handleInvalidBlock(
//        block: BlockMessage,
//        status: InvalidBlock,
//        dag: BlockDagRepresentation[F]
//    ): F[BlockDagRepresentation[F]]                                                = ???
//    override def calculateMissingDependencies(b: BlockMessage): F[List[BlockHash]] = ???
//    override def getReadyToBeAdded: F[List[BlockMessage]]                          = ???
//  }

}
