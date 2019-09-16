package coop.rchain.casper.api

import cats.Monad
import cats.effect.concurrent.Semaphore
import cats.implicits._

import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.engine._
import EngineCell._
import cats.effect.{Concurrent, Sync}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper._
import coop.rchain.casper.api.BlockAPI.ApiErr
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol._
import coop.rchain.casper.util._
import coop.rchain.casper.util.ConstructDeploy.basicDeployData
import coop.rchain.casper.util.rholang._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared.{Cell, Log, Time}
import coop.rchain.shared.scalatestcontrib._

import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{EitherValues, FlatSpec, Matchers}
import scala.concurrent.duration._

import coop.rchain.metrics

class CreateBlockAPITest extends FlatSpec with Matchers with EitherValues {
  import GenesisBuilder._
  import HashSetCasperTestNode.Effect

  def createBlock(blockApiLock: Semaphore[Task])(engineCell: Cell[Task, Engine[Task]])(
      implicit log: Log[Task],
      metrics: Metrics[Task],
      span: Span[Task]
  ): Task[Either[String, DeployServiceResponse]] =
    BlockAPI.createBlock[Task](blockApiLock)(
      Sync[Task],
      Concurrent[Task],
      engineCell,
      log,
      metrics,
      span
    )

  val genesisParameters = buildGenesisParameters(
    bondsFunction = _.zip(List(10L, 10L, 10L, 10L)).toMap
  )
  val genesis = buildGenesis(genesisParameters)

  implicit val scheduler = Scheduler.fixedPool("three-threads", 3)

  "createBlock" should "not allow simultaneous calls" in effectTest {
    implicit val metricsEff: Metrics[Effect] = new metrics.Metrics.MetricsNOP[Effect]
    implicit val logEff                      = new LogStub[Effect]
    implicit val spanEff                     = NoopSpan[Effect]
    implicit val time = new Time[Task] {
      private val timer                               = Task.timer
      def currentMillis: Task[Long]                   = timer.clock.realTime(MILLISECONDS)
      def nanoTime: Task[Long]                        = timer.clock.monotonic(NANOSECONDS)
      def sleep(duration: FiniteDuration): Task[Unit] = timer.sleep(duration)
    }
    HashSetCasperTestNode.standaloneEff(genesis).use { node =>
      val casper = new SleepingMultiParentCasperImpl[Effect](node.casperEff)
      val deploys = List(
        "@0!(0) | for(_ <- @0){ @1!(1) }",
        "for(_ <- @1){ @2!(2) }"
      ).map(ConstructDeploy.sourceDeploy(_, timestamp = System.currentTimeMillis()))

      def createBlock(deploy: DeployData, blockApiLock: Semaphore[Effect])(
          implicit engineCell: EngineCell[Effect]
      ): Effect[ApiErr[DeployServiceResponse]] =
        for {
          _ <- BlockAPI.deploy[Effect](deploy)
          r <- BlockAPI.createBlock[Effect](blockApiLock)
        } yield r

      def testProgram(blockApiLock: Semaphore[Effect])(
          implicit engineCell: EngineCell[Effect]
      ): Effect[
        (
            ApiErr[DeployServiceResponse],
            ApiErr[DeployServiceResponse],
            ApiErr[DeployServiceResponse]
        )
      ] =
        for {
          t1 <- createBlock(deploys.head, blockApiLock).start
          _  <- Time[Task].sleep(2.second)
          t2 <- createBlock(deploys.last, blockApiLock).start //should fail because other not done
          t3 <- createBlock(deploys.last, blockApiLock).start //should fail because other not done
          r1 <- t1.join.attempt
          r2 <- t2.join.attempt
          r3 <- t3.join.attempt
        } yield (r1.right.value, r2.right.value, r3.right.value)

      val (response1, response2, response3) = (for {
        engine       <- new EngineWithCasper[Task](casper).pure[Task]
        engineCell   <- Cell.mvarCell[Task, Engine[Task]](engine)
        blockApiLock <- Semaphore[Effect](1)
        result       <- testProgram(blockApiLock)(engineCell)
      } yield result).unsafeRunSync

      response1 shouldBe a[Right[_, DeployServiceResponse]]
      response2 shouldBe a[Left[_, DeployServiceResponse]]
      response3 shouldBe a[Left[_, DeployServiceResponse]]
      response2.left.value shouldBe "Error: There is another propose in progress."
      response3.left.value shouldBe "Error: There is another propose in progress."

      ().pure[Effect]
    }
  }

  it should "not allow proposals without enough new blocks from other validators" in effectTest {
    HashSetCasperTestNode
      .networkEff(genesis, networkSize = 4, synchronyConstraintThreshold = 1d / 3d)
      .use {
        case nodes @ n1 +: n2 +: _ +: _ +: Seq() =>
          import n1.{logEff, metricEff, span, timeEff}
          val engine = new EngineWithCasper[Task](n1.casperEff)
          for {
            deploys      <- (0 until 3).toList.traverse(i => basicDeployData[Task](i))
            engineCell   <- Cell.mvarCell[Task, Engine[Task]](engine)
            blockApiLock <- Semaphore[Effect](1)

            b1 <- n1.publishBlock(deploys(0))(nodes: _*)
            b2 <- n2.publishBlock(deploys(1))(nodes: _*)

            _        <- n1.casperEff.addDeploy(deploys(2))
            b3Status <- createBlock(blockApiLock)(engineCell)

            _ = b3Status.left.value should include(
              "Must wait for more blocks from other validators"
            )
          } yield ()
      }
  }

  it should "allow proposals with enough new blocks from other validators" in effectTest {
    HashSetCasperTestNode
      .networkEff(genesis, networkSize = 4, synchronyConstraintThreshold = 1d / 3d)
      .use {
        case nodes @ n1 +: n2 +: n3 +: _ +: Seq() =>
          import n1.{logEff, metricEff, span, timeEff}
          val engine = new EngineWithCasper[Task](n1.casperEff)
          for {
            deploys      <- (0 until 4).toList.traverse(i => basicDeployData[Task](i))
            engineCell   <- Cell.mvarCell[Task, Engine[Task]](engine)
            blockApiLock <- Semaphore[Effect](1)

            b1 <- n1.publishBlock(deploys(0))(nodes: _*)
            b2 <- n2.publishBlock(deploys(1))(nodes: _*)
            b3 <- n3.publishBlock(deploys(2))(nodes: _*)

            _        <- n1.casperEff.addDeploy(deploys(3))
            b4Status <- createBlock(blockApiLock)(engineCell)

            _ = b4Status shouldBe a[Right[_, DeployServiceResponse]]
          } yield ()
      }
  }
}

private class SleepingMultiParentCasperImpl[F[_]: Monad: Time](underlying: MultiParentCasper[F])
    extends MultiParentCasper[F] {

  def addBlock(
      b: BlockMessage,
      handleDoppelganger: (BlockMessage, Validator) => F[Unit]
  ): F[ValidBlockProcessing]                                  = underlying.addBlock(b, ignoreDoppelgangerCheck[F])
  def contains(blockHash: BlockHash): F[Boolean]              = underlying.contains(blockHash)
  def deploy(d: DeployData): F[Either[DeployError, DeployId]] = underlying.deploy(d)
  def estimator(dag: BlockDagRepresentation[F]): F[IndexedSeq[BlockHash]] =
    underlying.estimator(dag)
  def blockDag: F[BlockDagRepresentation[F]] = underlying.blockDag
  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] =
    underlying.normalizedInitialFault(weights)
  def lastFinalizedBlock: F[BlockMessage]     = underlying.lastFinalizedBlock
  def getRuntimeManager: F[RuntimeManager[F]] = underlying.getRuntimeManager
  def fetchDependencies: F[Unit]              = underlying.fetchDependencies

  override def createBlock: F[CreateBlockStatus] =
    for {
      result <- underlying.createBlock
      _      <- implicitly[Time[F]].sleep(5.seconds)
    } yield result

}
