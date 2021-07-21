package coop.rchain.casper.merging

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.{EventLogIndex, StateChange}
import coop.rchain.rspace.trace.{Consume, Produce}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

class DeployChainIndexSpec extends FlatSpec with Matchers {

  // factories
  def randomBlake: Blake2b256Hash =
    Blake2b256Hash.fromByteArray(Array.fill(32)((scala.util.Random.nextInt(256) - 128).toByte))
  implicit val c = Concurrent[Task]

  val preState  = randomBlake
  val postState = randomBlake

  var preStateUsed  = randomBlake
  var postStateUsed = randomBlake

  def computeStateChangeF(
      eventLogIndex: EventLogIndex,
      preState: Blake2b256Hash,
      postState: Blake2b256Hash
  ): Task[StateChange] = Sync[Task].delay {
    preStateUsed = preState
    postStateUsed = postState
    StateChange.empty
  }

  val dummyProduce = Produce(randomBlake, randomBlake, false)
  val dummyConsume = Consume(Seq(randomBlake), randomBlake, false)
  val eli1 = EventLogIndex(
    Set(dummyProduce),
    Set(dummyProduce),
    Set(dummyProduce),
    Set(dummyProduce),
    Set(dummyProduce),
    Set(dummyProduce),
    Set(dummyConsume),
    Set(dummyConsume),
    Set(dummyConsume)
  )
  val eli2 = eli1.copy()

  val dI1 = DeployIndex(ByteString.copyFromUtf8("dI1"), 1, List(), _ => eli1.pure).runSyncUnsafe()
  val dI2 = DeployIndex(ByteString.copyFromUtf8("dI2"), 2, List(), _ => eli2.pure).runSyncUnsafe()

  val dci = DeployChainIndex(
    deploys = Set(dI1, dI2),
    preStateHash = preState,
    postStateHash = postState,
    computeStateChangeF = computeStateChangeF
  ).runSyncUnsafe()

  "state changes" should "computed between post state and pre state" in {
    preStateUsed shouldBe preState
    postStateUsed shouldBe postState
  }

  "resulting event log index" should "be a combination event logs of input deploy indices" in {
    dci.eventLogIndex shouldBe EventLogIndex.combine(dI1.eventLogIndex, dI2.eventLogIndex)
  }

  "deploy Ids and cost" should "populated correctly" in {
    dci.deploysWithCost shouldBe Set(
      DeployIdWithCost(dI1.deployId, dI1.cost),
      DeployIdWithCost(dI2.deployId, dI2.cost)
    )
  }
}
