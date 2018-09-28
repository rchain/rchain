package coop.rchain.casper.api

import cats.effect.{Sync, Timer}
import cats.{Functor, Id, Monad}
import cats.data.EitherT
import cats.implicits._
import cats.mtl.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper._
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.protocol._
import coop.rchain.casper.util._
import coop.rchain.casper.util.rholang._
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.casper.Estimator.Validator
import monix.eval.Task
import monix.execution.Scheduler
import coop.rchain.shared.AttemptOps._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.p2p.EffectsTestInstances._
import coop.rchain.casper.MultiParentCasperRef.MultiParentCasperRef
import coop.rchain.rholang.interpreter.accounting

import scala.concurrent.duration._
import org.scalatest.{FlatSpec, Matchers}

class CreateBlockAPITest extends FlatSpec with Matchers {
  import HashSetCasperTest._
  import HashSetCasperTestNode.Effect

  private val (validatorKeys, validators) = (1 to 4).map(_ => Ed25519.newKeyPair).unzip
  private val bonds                       = createBonds(validators)
  private val genesis                     = createGenesis(bonds)

  implicit val timerEff: Timer[Effect] = Timer.deriveEitherT(Functor[Task], Task.timer)

  "createBlock" should "not allow simultaneous calls" in {
    implicit val scheduler = Scheduler.fixedPool("three-threads", 3)
    val node               = HashSetCasperTestNode.standaloneEff(genesis, validatorKeys.head)
    val casper             = new SleepingMultiParentCasperImpl[Effect](node.casperEff)
    val deploys = List(
      "@0!(0) | for(_ <- @0){ @1!(1) }",
      "for(_ <- @1){ @2!(2) }"
    ).map(ProtoUtil.sourceDeploy(_, System.currentTimeMillis(), accounting.MAX_VALUE))

    implicit val logEff = new LogStub[Effect]
    def testProgram(
        implicit casperRef: MultiParentCasperRef[Effect]
    ): Effect[(DeployServiceResponse, DeployServiceResponse)] = EitherT.liftF(
      for {
        t1 <- (BlockAPI.deploy[Effect](deploys.head) *> BlockAPI.createBlock[Effect]).value.fork
        _  <- implicitly[Timer[Task]].sleep(2.second)
        t2 <- (BlockAPI.deploy[Effect](deploys.last) *> BlockAPI
               .createBlock[Effect]).value.fork //should fail because other not done
        r1 <- t1.join
        r2 <- t2.join
      } yield (r1.right.get, r2.right.get)
    )

    val (response1, response2) = (for {
      casperRef <- MultiParentCasperRef.of[Effect]
      _         <- casperRef.set(casper)
      result    <- testProgram(casperRef)
    } yield result).value.unsafeRunSync.right.get

    response1.success shouldBe true
    response2.success shouldBe false
    response2.message shouldBe "Error: There is another propose in progress."

    node.tearDown()
  }
}

private class SleepingMultiParentCasperImpl[F[_]: Monad: Timer](underlying: MultiParentCasper[F])
    extends MultiParentCasper[F] {

  def addBlock(b: BlockMessage): F[BlockStatus]             = underlying.addBlock(b)
  def contains(b: BlockMessage): F[Boolean]                 = underlying.contains(b)
  def deploy(d: DeployData): F[Either[Throwable, Unit]]     = underlying.deploy(d)
  def estimator(dag: BlockDag): F[IndexedSeq[BlockMessage]] = underlying.estimator(dag)
  def blockDag: F[BlockDag]                                 = underlying.blockDag
  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] =
    underlying.normalizedInitialFault(weights)
  def lastFinalizedBlock: F[BlockMessage]          = underlying.lastFinalizedBlock
  def storageContents(hash: ByteString): F[String] = underlying.storageContents(hash)
  def getRuntimeManager: F[Option[RuntimeManager]] = underlying.getRuntimeManager

  override def createBlock: F[CreateBlockStatus] =
    for {
      result <- underlying.createBlock
      _      <- implicitly[Timer[F]].sleep(5.seconds)
    } yield result

}
