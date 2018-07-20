package coop.rchain.blockstorage

import cats._
import cats.effect._
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.{BlockMessage, Header}
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.MetricsNOP
import org.scalacheck._
import org.scalactic.anyvals.PosInt
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.language.higherKinds

import Gen._
import Arbitrary.arbitrary

trait BlockStoreTest
    extends FlatSpecLike
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterEach
    with BeforeAndAfterAll {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(100))

  override def beforeEach(): Unit = {}

  override def afterEach(): Unit = {}

  def bm(bh: BlockHash, v: Long, ts: Long): BlockMessage =
    BlockMessage(blockHash = bh).withHeader(Header().withVersion(v).withTimestamp(ts))

  def withStore[R](f: BlockStore[Id] => R): R

  private[this] val blockHashGen: Gen[BlockHash] = for {
    testKey <- arbitrary[String]
  } yield (ByteString.copyFrom(testKey, "utf-8"))

  private[this] val blockHashesSetGen: Gen[Set[BlockHash]] =
    Gen.buildableOf[Set[BlockHash], BlockHash](blockHashGen)

  "Block Store" should "return None on get while it's empty" in withStore { store =>
    store.get(blockHashGen.sample.get) shouldBe None
  }

  it should "return Some(message) on get for a published key" in {
    forAll(blockHashesSetGen) { blockHashes =>
      withStore { store =>
        val items = blockHashes.map { hash =>
          val msg: BlockMessage = bm(hash, 100L, 10000L)
          (hash, msg)
        }
        items.foreach { case (k, v) => store.put(k, v) }
        items.foreach {
          case (k, v) =>
            store.get(k) shouldBe Some(v)
        }
        store.asMap().size shouldEqual items.size
      }
    }
  }

  it should "overwrite existing value" in
    forAll(blockHashesSetGen) { blockHashes =>
      withStore { store =>
        val items = blockHashes map { hash =>
          val msg1: BlockMessage = bm(hash, 100L, 10000L)
          val msg2: BlockMessage = bm(hash, 200L, 20000L)
          (hash, msg1, msg2)
        }
        items.foreach { case (k, v1, _) => store.put(k, v1) }
        items.foreach { case (k, v1, _) => store.get(k) shouldBe Some(v1) }
        items.foreach { case (k, _, v2) => store.put(k, v2) }
        items.foreach { case (k, _, v2) => store.get(k) shouldBe Some(v2) }

        store.asMap().size shouldEqual items.size
      }
    }
}

class InMemBlockStoreTest extends BlockStoreTest {
  override def withStore[R](f: BlockStore[Id] => R): R = {

    implicit val bracket              = InMemBlockStore.bracketId
    implicit val metrics: Metrics[Id] = new MetricsNOP[Id]()(bracket)

    val store = BlockStore.createMapBased[Id]
    f(store)
  }
}
