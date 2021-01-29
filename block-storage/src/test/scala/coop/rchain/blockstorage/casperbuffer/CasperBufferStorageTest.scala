package coop.rchain.blockstorage.casperbuffer

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.codecs.{codecBlockHash, codecBlockHashSet}
import coop.rchain.metrics.Metrics
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log.NOPLog
import coop.rchain.store.InMemoryStoreManager
import monix.eval.Task
import org.scalatest.{FlatSpecLike, Matchers}
import monix.execution.Scheduler.Implicits.global
import coop.rchain.shared.syntax._
import coop.rchain.models.blockImplicits._

class CasperBufferStorageTest extends FlatSpecLike with Matchers {

  implicit val log     = new NOPLog[Task]()
  implicit val metrics = new Metrics.MetricsNOP[Task]

  val kvsManager = InMemoryStoreManager[Task]

  val underlyingStore = kvsManager
    .database[BlockHash, Set[BlockHash]](
      "parents-map",
      codecBlockHash,
      codecBlockHashSet
    )
    .runSyncUnsafe()

  val A = blockHashGen.sample.get
  val B = blockHashGen.sample.get
  val C = blockHashGen.sample.get
  val D = blockHashGen.sample.get

  underlyingStore.put(C, Set(D)).runSyncUnsafe()

  val casperBuffer = CasperBufferKeyValueStorage
    .create[Task](kvsManager)
    .runSyncUnsafe()

  it should "be able to restore state on startup" in {
    casperBuffer.getParents(C).runSyncUnsafe() shouldBe Some(Set(D))
    casperBuffer.getChildren(D).runSyncUnsafe() shouldBe Some(Set(C))
  }

  "add relation" should "change parents set and children set" in {
    casperBuffer.addRelation(A, B).runSyncUnsafe()
    casperBuffer.getParents(B).runSyncUnsafe().get shouldBe Set(A)
    casperBuffer.getChildren(A).runSyncUnsafe().get shouldBe Set(B)
  }

  "block that has no parents" should "be pendant" in {
    casperBuffer.addRelation(A, B).runSyncUnsafe()
    casperBuffer.isPendant(A).runSyncUnsafe() shouldBe true
  }

  "when removed hash A is the last parent for hash B, key B" should "be removed from parents store" in {
    underlyingStore.get(B).runSyncUnsafe() shouldBe Some(Set(A))
    casperBuffer.remove(A).runSyncUnsafe()
    underlyingStore.get(B).runSyncUnsafe() shouldBe None
  }

  "when removed hash A is the last parent for hash B, B" should "become pendant" in {
    casperBuffer.isPendant(B).runSyncUnsafe() shouldBe true
  }
}
