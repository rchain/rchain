package coop.rchain.blockstorage

import scala.language.higherKinds

import cats._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.{BlockMessage, Header}
import coop.rchain.rspace.Context
import coop.rchain.shared.PathOps._
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalactic.anyvals.PosInt
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.util.control.NonFatal

trait BlockStoreTest
    extends FlatSpecLike
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterAll {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(100))

  private[this] def toBlockMessage(bh: String, v: Long, ts: Long): BlockMessage =
    BlockMessage(blockHash = bh)
      .withHeader(Header().withVersion(v).withTimestamp(ts))

  private[this] implicit def liftToBlockHash(s: String): BlockHash = ByteString.copyFromUtf8(s)
  private[this] implicit def liftToBlockStoreElement(
      s: (String, BlockMessage)): (BlockHash, BlockMessage) =
    (ByteString.copyFromUtf8(s._1), s._2)

  private[this] val blockHashGen: Gen[BlockHash] = for {
    testKey <- arbitrary[String].suchThat(_.nonEmpty)
  } yield ByteString.copyFromUtf8(testKey)

  private[this] implicit val arbitraryHash: Arbitrary[BlockHash] = Arbitrary(blockHashGen)

  private[this] val blockStoreElementGen: Gen[(String, BlockMessage)] =
    for {
      hash      <- arbitrary[BlockHash]
      version   <- arbitrary[Long]
      timestamp <- arbitrary[Long]
    } yield
      (hash.toStringUtf8,
       BlockMessage(blockHash = hash)
         .withHeader(Header().withVersion(version).withTimestamp(timestamp)))

  private[this] val blockStoreElementsGen: Gen[List[(String, BlockMessage)]] =
    distinctListOfGen(blockStoreElementGen)(_._1 == _._1)

  def withStore[R](f: BlockStore[Id] => R): R

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
    withStore { store =>
      forAll(blockStoreElementsGen, minSize(0), sizeRange(10)) { blockStoreElements =>
        val items = blockStoreElements
        items.foreach(store.put(_))
        items.foreach {
          case (k, v) =>
            store.get(k) shouldBe Some(v)
        }
        store.asMap().size shouldEqual items.size
        store.clear()
      }
    }
  }

  it should "discover keys by predicate" in {
    withStore { store =>
      forAll(blockStoreElementsGen, minSize(0), sizeRange(10)) { blockStoreElements =>
        val items = blockStoreElements
        items.foreach(store.put(_))
        items.foreach {
          case (k, v) =>
            val w = store.find(_ == ByteString.copyFrom(k.getBytes()))
            w should have size 1
            w.head._2 shouldBe v
        }
        store.asMap().size shouldEqual items.size
        store.clear()
      }
    }
  }

  it should "overwrite existing value" in
    withStore { store =>
      forAll(blockStoreElementsGen, minSize(0), sizeRange(10)) { blockStoreElements =>
        val items = blockStoreElements.map {
          case (hash, elem) =>
            (hash, elem, toBlockMessage(hash, 200L, 20000L))
        }
        items.foreach { case (k, v1, _) => store.put(k, v1) }
        items.foreach { case (k, v1, _) => store.get(k) shouldBe Some(v1) }
        items.foreach { case (k, _, v2) => store.put(k, v2) }
        items.foreach { case (k, _, v2) => store.get(k) shouldBe Some(v2) }

        store.asMap().size shouldEqual items.size
        store.clear()
      }
    }

  it should "rollback the transaction on error" in {
    withStore { store =>
      store.asMap().size shouldEqual 0
      def elem = {
        blockStoreElementGen.sample.get
        throw new RuntimeException("msg")
      }

      a[RuntimeException] shouldBe thrownBy {
        store.put { elem }
      }
      store.asMap().size shouldEqual 0
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

  import java.nio.file.{Files, Path}

  private[this] def mkTmpDir(): Path = Files.createTempDirectory("block-store-test-")
  private[this] val mapSize: Long    = 100L * 1024L * 1024L * 4096L

  override def withStore[R](f: BlockStore[Id] => R): R = {
    val dbDir = mkTmpDir()
    val env   = Context.env(dbDir, mapSize)
    val store = LMDBBlockStore.createWithId(env, dbDir)
    try {
      f(store)
    } finally {
      env.close()
      dbDir.recursivelyDelete
    }
  }
}
