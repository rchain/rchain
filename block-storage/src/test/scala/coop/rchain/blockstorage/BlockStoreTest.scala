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

  private[this] implicit val arbitraryHash: Arbitrary[BlockHash] = Arbitrary(blockHashGen)

  private[this] val blockStoreElementGen: Gen[(BlockHash, BlockMessage)] =
    for {
      hash      <- arbitrary[BlockHash]
      version   <- arbitrary[Long]
      timestamp <- arbitrary[Long]
    } yield
      ((hash,
        BlockMessage(blockHash = hash)
          .withHeader(Header().withVersion(version).withTimestamp(timestamp))))

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
          case Some(t) if seen.filter(comp(t, _)).isEmpty =>
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
        //blockStoreElements.map(_._1).toSet.size shouldBe items.size
        items.foreach { case (k, v) => store.put(k, v) }
        items.foreach {
          case (k, v) =>
            store.get(k) shouldBe Some(v)
        }
      //store.asMap().size shouldEqual items.size
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

      //store.asMap().size shouldEqual items.size
      }
    }
}

class InMemBlockStoreTest extends BlockStoreTest {
  override def withStore[R](f: BlockStore[Id] => R): R = {

    val sync: Sync[Id] = new Sync[Id] {
      // Members declared in cats.Applicative
      def pure[A](x: A): cats.Id[A] = implicitly[Applicative[Id]].pure(x)

      // Members declared in cats.ApplicativeError
      def handleErrorWith[A](fa: cats.Id[A])(f: Throwable => cats.Id[A]): cats.Id[A] = ???
      def raiseError[A](e: Throwable): cats.Id[A]                                    = ???

      // Members declared in cats.effect.Bracket
      def bracketCase[A, B](acquire: cats.Id[A])(use: A => cats.Id[B])(
          release: (A, cats.effect.ExitCase[Throwable]) => cats.Id[Unit]): cats.Id[B] = ???

      // Members declared in cats.FlatMap
      def flatMap[A, B](fa: cats.Id[A])(f: A => cats.Id[B]): cats.Id[B] =
        implicitly[Monad[Id]].flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => cats.Id[Either[A, B]]): cats.Id[B] = ???

      // Members declared in cats.effect.Sync
      def suspend[A](thunk: => cats.Id[A]): cats.Id[A] = thunk
    }

    implicit val ref: Ref[Id, Map[BlockHash, BlockMessage]] =
      Ref.of[Id, Map[BlockHash, BlockMessage]](Map.empty)(sync)
    //new Ref[Id, Map[BlockHash, BlockMessage]] {}

    implicit val metrics: Metrics[Id] = new MetricsNOP[Id]()(sync)

    val store = BlockStore.createMapBased[Id](sync, ref, metrics)
    f(store)
  }
}
