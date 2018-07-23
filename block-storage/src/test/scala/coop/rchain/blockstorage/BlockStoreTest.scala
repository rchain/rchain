package coop.rchain.blockstorage

import cats._
import cats.effect._
import cats.effect.concurrent._
import cats.implicits._
import cats.effect.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.{BlockMessage, Header}
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.rspace.Context
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
    with BeforeAndAfterEach {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(1))

  def bm(bh: BlockHash, v: Long, ts: Long): BlockMessage =
    BlockMessage(blockHash = bh).withHeader(Header().withVersion(v).withTimestamp(ts))

  def withStore[R](f: BlockStore[Id] => R): R

  private[this] val blockHashGen: Gen[BlockHash] = for {
    testKey <- arbitrary[String]
  } yield ByteString.copyFrom(testKey, "utf-8")

  private[this] implicit val arbitraryHash: Arbitrary[BlockHash] = Arbitrary(blockHashGen)

  private[this] val blockStoreElementGen: Gen[(BlockHash, BlockMessage)] =
    for {
      hash      <- arbitrary[BlockHash]
      version   <- arbitrary[Long]
      timestamp <- arbitrary[Long]
    } yield
      (hash,
       BlockMessage(blockHash = hash)
         .withHeader(Header().withVersion(version).withTimestamp(timestamp)))

  private[this] val blockStoreElementsGen: Gen[List[(BlockHash, BlockMessage)]] =
    distinctListOfGen(blockStoreElementGen)(_._1 == _._1)

  // TODO: move to `shared` along with code in coop.rchain.rspace.test.ArbitraryInstances
  /**
   Credit: https://gist.github.com/etorreborre/d0616e704ed85d7276eb12b025df8ab0

   Distinct list of elements from a given arbitrary
    */
  def distinctListOf[T: Arbitrary] =
    distinctListOfGen(arbitrary[T])(_ == _)

  /**
   Distinct list of elements from a given generator
   with a maximum number of elements to discard
    */
  def distinctListOfGen[T](gen: Gen[T], maxDiscarded: Int = 1000)(
      comp: (T, T) => Boolean): Gen[List[T]] = {
    val seen      = new scala.collection.mutable.ListBuffer[T]
    var discarded = 0

    Gen.sized { size =>
      if (size == seen.size) seen.toList
      else {
        while (seen.size <= size && discarded < maxDiscarded) gen.sample match {
          case Some(t) if !seen.exists(comp(t, _)) =>
            seen.+=:(t)
          case _ => discarded += 1
        }
        seen.toList
      }
    }
  }

  "Block Store" should "return None on get while it's empty" in withStore { store =>
    store.get(blockHashGen.sample.get) shouldBe None
  }

  it should "return Some(message) on get for a published key" in {
    forAll(blockStoreElementsGen) { blockStoreElements =>
      withStore { store =>
        val items = blockStoreElements
        items.foreach(store.put(_))
        items.foreach {
          case (k, v) =>
            store.get(k) shouldBe Some(v)
        }
        store.asMap().size shouldEqual items.size
      }
    }
  }

  it should "overwrite existing value" in
    forAll(blockStoreElementsGen) { blockStoreElements =>
      withStore { store =>
        val items = blockStoreElements.map {
          case (hash, elem) =>
            val msg2: BlockMessage = bm(hash, 200L, 20000L)
            (hash, elem, msg2)
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
    val store = InMemBlockStore.createWithId
    f(store)
  }
}

class LMDBBlockStoreTest extends BlockStoreTest {
  override def withStore[R](f: BlockStore[Id] => R): R = {

    import java.nio.file.{Files, Path}
    val dbDir: Path   = Files.createTempDirectory("block-store-test-")
    val mapSize: Long = 1024L * 1024L * 4096L
    val env           = Context.env(dbDir, mapSize)
    val store         = LMDBBlockStore.createWithId(env, dbDir)
    try {
      f(store)
    } finally {
      env.close()
    }
  }
}
