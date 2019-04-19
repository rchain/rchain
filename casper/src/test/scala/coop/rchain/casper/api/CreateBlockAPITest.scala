package coop.rchain.casper.api

import scala.concurrent.duration._

import cats.Monad
import cats.data.EitherT
import cats.effect.concurrent.Semaphore
import cats.implicits._

import coop.rchain.blockstorage.BlockDagRepresentation
import coop.rchain.casper._
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol._
import coop.rchain.casper.util._
import coop.rchain.casper.util.rholang._
import coop.rchain.casper.Estimator.Validator
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.casper.api.BlockAPI.ApiErr
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.shared.Time

import com.google.protobuf.ByteString
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{FlatSpec, Matchers}

class CreateBlockAPITest extends FlatSpec with Matchers {
  import MultiParentCasperTestUtil._
  import HashSetCasperTestNode.Effect

  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val bonds                       = createBonds(validators)
  private val genesis                     = createGenesis(bonds)

  "createBlock" should "not allow simultaneous calls" in {
    implicit val logEff    = new LogStub[Effect]
    implicit val scheduler = Scheduler.fixedPool("three-threads", 3)
    implicit val time = new Time[Task] {
      private val timer                               = Task.timer
      def currentMillis: Task[Long]                   = timer.clock.realTime(MILLISECONDS)
      def nanoTime: Task[Long]                        = timer.clock.monotonic(NANOSECONDS)
      def sleep(duration: FiniteDuration): Task[Unit] = timer.sleep(duration)
    }
    val node   = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    val casper = new SleepingMultiParentCasperImpl[Effect](node.casperEff)
    val deploys = List(
      "@0!(0) | for(_ <- @0){ @1!(1) }",
      "for(_ <- @1){ @2!(2) }"
    ).map(ConstructDeploy.sourceDeploy(_, System.currentTimeMillis(), accounting.MAX_VALUE))

    def createBlock(deploy: DeployData, blockApiLock: Semaphore[Effect])(
        implicit casperRef: MultiParentCasperRef[Effect]
    ): Effect[ApiErr[DeployServiceResponse]] =
      for {
        _ <- BlockAPI.deploy[Effect](deploy)
        r <- BlockAPI.createBlock[Effect](blockApiLock)
      } yield r

    def testProgram(blockApiLock: Semaphore[Effect])(
        implicit casperRef: MultiParentCasperRef[Effect]
    ): Effect[
      (ApiErr[DeployServiceResponse], ApiErr[DeployServiceResponse], ApiErr[DeployServiceResponse])
    ] =
      EitherT.liftF(
        for {
          t1 <- createBlock(deploys.head, blockApiLock).value.start
          _  <- Time[Task].sleep(2.second)
          t2 <- createBlock(deploys.last, blockApiLock).value.start //should fail because other not done
          t3 <- createBlock(deploys.last, blockApiLock).value.start //should fail because other not done
          r1 <- t1.join
          r2 <- t2.join
          r3 <- t3.join
        } yield (r1.right.get, r2.right.get, r3.right.get)
      )

    val (response1, response2, response3) = (for {
      casperRef    <- MultiParentCasperRef.of[Effect]
      _            <- casperRef.set(casper)
      blockApiLock <- Semaphore[Effect](1)
      result       <- testProgram(blockApiLock)(casperRef)
    } yield result).value.unsafeRunSync.right.get

    response1 shouldBe a[Right[_, DeployServiceResponse]]
    response2 shouldBe a[Left[_, DeployServiceResponse]]
    response3 shouldBe a[Left[_, DeployServiceResponse]]
    response2.left.get shouldBe "Error: There is another propose in progress."
    response3.left.get shouldBe "Error: There is another propose in progress."

    node.tearDown()
  }
}

private class SleepingMultiParentCasperImpl[F[_]: Monad: Time](underlying: MultiParentCasper[F])
    extends MultiParentCasper[F] {

  def addBlock(
      b: BlockMessage,
      handleDoppelganger: (BlockMessage, Validator) => F[Unit]
  ): F[BlockStatus]                                       = underlying.addBlock(b, ignoreDoppelgangerCheck[F])
  def contains(b: BlockMessage): F[Boolean]               = underlying.contains(b)
  def deploy(d: DeployData): F[Either[DeployError, Unit]] = underlying.deploy(d)
  def estimator(dag: BlockDagRepresentation[F]): F[IndexedSeq[BlockHash]] =
    underlying.estimator(dag)
  def blockDag: F[BlockDagRepresentation[F]] = underlying.blockDag
  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] =
    underlying.normalizedInitialFault(weights)
  def lastFinalizedBlock: F[BlockMessage]             = underlying.lastFinalizedBlock
  def storageContents(hash: ByteString): F[String]    = underlying.storageContents(hash)
  def getRuntimeManager: F[Option[RuntimeManager[F]]] = underlying.getRuntimeManager
  def fetchDependencies: F[Unit]                      = underlying.fetchDependencies

  override def createBlock: F[CreateBlockStatus] =
    for {
      result <- underlying.createBlock
      _      <- implicitly[Time[F]].sleep(5.seconds)
    } yield result

}
