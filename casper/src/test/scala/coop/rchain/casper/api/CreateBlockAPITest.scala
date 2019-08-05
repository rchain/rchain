package coop.rchain.casper.api

import cats.Monad
import cats.effect.concurrent.Semaphore
import cats.implicits._
import coop.rchain.blockstorage.BlockDagRepresentation
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.engine._
import EngineCell._
import coop.rchain.casper._
import coop.rchain.casper.api.BlockAPI.ApiErr
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol._
import coop.rchain.casper.util._
import coop.rchain.casper.util.rholang._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.metrics.Span.TraceId
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.shared.{Cell, Time}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class CreateBlockAPITest extends FlatSpec with Matchers {
  import GenesisBuilder._
  import HashSetCasperTestNode.Effect

  val genesis                            = buildGenesis()
  implicit val spanEff: NoopSpan[Effect] = NoopSpan[Effect]

  "createBlock" should "not allow simultaneous calls" in {
    implicit val logEff    = new LogStub[Effect]
    implicit val scheduler = Scheduler.fixedPool("three-threads", 3)
    implicit val spanEff   = NoopSpan[Effect]
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
          _ <- BlockAPI.deploy[Effect](deploy, traceId)
          r <- BlockAPI.createBlock[Effect](blockApiLock, traceId)
        } yield r

      def testProgram(blockApiLock: Semaphore[Effect])(
          implicit engineCell: EngineCell[Effect]
      ): Effect[
        (
            Either[Throwable, ApiErr[DeployServiceResponse]],
            Either[Throwable, ApiErr[DeployServiceResponse]],
            Either[Throwable, ApiErr[DeployServiceResponse]]
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
        } yield (r1, r2, r3)

      val (response1, response2, response3) = (for {
        engine       <- new EngineWithCasper[Task](casper).pure[Task]
        engineCell   <- Cell.mvarCell[Task, Engine[Task]](engine)
        blockApiLock <- Semaphore[Effect](1)
        result       <- testProgram(blockApiLock)(engineCell)
      } yield result).unsafeRunSync

      response1 shouldBe a[Right[_, DeployServiceResponse]]
      response2 shouldBe a[Left[_, DeployServiceResponse]]
      response3 shouldBe a[Left[_, DeployServiceResponse]]
      response2.left.map(_.getMessage) shouldBe "Error: There is another propose in progress."
      response3.left.map(_.getMessage) shouldBe "Error: There is another propose in progress."

      ().pure[Effect]
    }
  }
}

private class SleepingMultiParentCasperImpl[F[_]: Monad: Time](underlying: MultiParentCasper[F])
    extends MultiParentCasper[F] {
  def addBlock(b: BlockMessage, handleDoppelganger: (BlockMessage, Validator) => F[Unit])(
      implicit traceId: TraceId
  ): F[BlockStatus] =
    underlying.addBlock(b, ignoreDoppelgangerCheck[F])(traceId)
  def contains(blockHash: BlockHash): F[Boolean]              = underlying.contains(blockHash)
  def deploy(d: DeployData): F[Either[DeployError, DeployId]] = underlying.deploy(d)
  def estimator(
      dag: BlockDagRepresentation[F]
  )(implicit traceId: TraceId): F[IndexedSeq[BlockHash]] =
    underlying.estimator(dag)
  def blockDag: F[BlockDagRepresentation[F]] = underlying.blockDag
  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] =
    underlying.normalizedInitialFault(weights)
  def lastFinalizedBlock(implicit traceId: TraceId): F[BlockMessage] = underlying.lastFinalizedBlock
  def getRuntimeManager: F[RuntimeManager[F]]                        = underlying.getRuntimeManager
  def fetchDependencies: F[Unit]                                     = underlying.fetchDependencies

  override def createBlock(implicit traceId: TraceId): F[CreateBlockStatus] =
    for {
      result <- underlying.createBlock(traceId)
      _      <- implicitly[Time[F]].sleep(5.seconds)
    } yield result

}
