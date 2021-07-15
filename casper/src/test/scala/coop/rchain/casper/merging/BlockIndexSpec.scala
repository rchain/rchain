package coop.rchain.casper.merging

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.casper.merging.DeployIndex._
import coop.rchain.casper.protocol.ProcessedSystemDeploy.{Failed, Succeeded}
import coop.rchain.casper.protocol._
import coop.rchain.casper.util.EventConverter
import coop.rchain.crypto.signatures.{Secp256k1, SignaturesAlg, Signed}
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.models._
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.{EventLogIndex, StateChange}
import coop.rchain.rspace.trace.Consume
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

class BlockIndexSpec extends FlatSpec with Matchers {

  // factories
  def random32BS: ByteString =
    ByteString.copyFrom(Array.fill(32)((scala.util.Random.nextInt(256) - 128).toByte))

  def randomDeploy(events: List[Event], failed: Boolean) =
    ProcessedDeploy(
      Signed(
        DeployData("", 0, 0, 0, 0),
        SignaturesAlg(Secp256k1.name).get,
        PrivateKey(random32BS)
      ),
      PCost(1),
      events,
      isFailed = failed
    )

  // for testing dependent deploys
  // events
  val masterConsume   = ConsumeEvent(List(random32BS), random32BS, persistent = false)
  val randomConsume   = ConsumeEvent(List(random32BS), random32BS, persistent = false)
  val dependentComm   = CommEvent(masterConsume, List(), List())
  val independentComm = CommEvent(randomConsume, List(), List())
  // deploys with events
  val masterDeploy      = randomDeploy(List(masterConsume), false)
  val dependentDeploy   = randomDeploy(List(dependentComm), false)
  val independentDeploy = randomDeploy(List(independentComm), false)
  val invalidDeploy     = randomDeploy(List(), true)

  // for testing systemDeploys
  val slashData = SlashSystemDeployData(ByteString.EMPTY, PublicKey(ByteString.EMPTY))
  val successfulSystemDeploys = List(
    ProcessedSystemDeploy.Succeeded(List(), CloseBlockSystemDeployData),
    ProcessedSystemDeploy.Succeeded(List(), slashData)
  )
  val failedSystemDeploys = Seq(ProcessedSystemDeploy.Failed(List.empty, ""))

  // create block to pass into indexer
  val rndBlock = getRandomBlock()
  val testBlock =
    rndBlock.copy(
      body = rndBlock.body
        .copy(
          deploys = masterDeploy +: dependentDeploy +: independentDeploy +: invalidDeploy +: rndBlock.body.deploys,
          systemDeploys = successfulSystemDeploys ++ failedSystemDeploys
        )
    )

  // create index for block
  implicit val c = Concurrent[Task]
  def indexDeployLogF(events: List[Event], preState: Blake2b256Hash) =
    Sync[Task].delay(events match {
      case List(v @ ConsumeEvent(_, _, _)) =>
        EventLogIndex.empty
          .copy(consumesLinearAndPeeks = Set(EventConverter.toRspaceEvent(v).asInstanceOf[Consume]))
      case List(_ @CommEvent(consume, _, _)) =>
        EventLogIndex.empty.copy(
          consumesProduced = Set(EventConverter.toRspaceEvent(consume).asInstanceOf[Consume])
        )
      case List() => EventLogIndex.empty
    })

  val index = BlockIndex(
    blockHash = testBlock.blockHash,
    usrProcessedDeploys = testBlock.body.deploys,
    sysProcessedDeploys = testBlock.body.systemDeploys,
    Blake2b256Hash.fromByteString(random32BS),
    Blake2b256Hash.fromByteString(random32BS),
    indexDeployLogF,
    (_: EventLogIndex, _: Blake2b256Hash, _: Blake2b256Hash) => StateChange.empty.pure
  ).runSyncUnsafe()

  // helper function
  val getIndicesContaining =
    (d: ByteString) => index.deployChains.filter(_.deploysWithCost.map(_.id).contains(d))

  require(testBlock.body.deploys.exists(_.isFailed), "bug in test, no failed deploys")
  require(testBlock.body.deploys.exists(d => (!d.isFailed)), "bug in test, no successful deploys")
  require(
    testBlock.body.systemDeploys.collect { case v: Succeeded => v }.nonEmpty,
    "bug in test, no successful system deploys"
  )
  require(
    testBlock.body.systemDeploys.collect { case v: Failed => v }.nonEmpty,
    "bug in test, no unsuccessful system deploys"
  )

  it should "attribute to correct block hash" in {
    testBlock.blockHash shouldBe index.blockHash
  }

  it should "index all successful user deploys" in {
    testBlock.body.deploys.toIterator
      .filterNot(_.isFailed)
      .map(_.deploy.sig)
      .map { deploySig =>
        index.deployChains.flatMap(_.deploysWithCost.map(_.id)).contains(deploySig)
      }
      .filter(_ == false)
      .toList
      .size shouldBe 0
  }

  it should "not index failed user deploys" in {
    getIndicesContaining(invalidDeploy.deploy.sig).size shouldBe 0
  }

  it should "index system deploys, those and only those succeed" in {
    getIndicesContaining(SYS_SLASH_DEPLOY_ID).size shouldBe 1       // slash deploy is succeed
    getIndicesContaining(SYS_CLOSE_BLOCK_DEPLOY_ID).size shouldBe 1 // close deploy is succeed
    getIndicesContaining(SYS_EMPTY_DEPLOY_ID).size shouldBe 0       // empty deploy failed
  }

  "a deploy" should "belong to only one deploy chain index" in {
    testBlock.body.deploys.toIterator
      .map(_.deploy.sig)
      .filterNot(_ == invalidDeploy.deploy.sig)
      .map(getIndicesContaining)
      .exists(_.size != 1) shouldBe false
  }

  "independent deploys" should "be put in different deploy chain indices" in {
    getIndicesContaining(masterDeploy.deploy.sig) !=
      getIndicesContaining(independentDeploy.deploy.sig) shouldBe true
  }

  "dependent deploys" should "be put in one deploy chain index" in {
    getIndicesContaining(masterDeploy.deploy.sig) shouldBe
      getIndicesContaining(dependentDeploy.deploy.sig)
  }
}
