package coop.rchain.blockstorage

import java.nio.file.Paths

import scala.language.higherKinds
import cats._
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.casper.protocol.{BlockMessage, Header}
import coop.rchain.rspace.Context
import coop.rchain.shared.PathOps._
import BlockGen.blockHashElementsGen
import coop.rchain.blockstorage.InMemBlockStore.emptyMapRef
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.catscontrib.TaskContrib._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.anyvals.PosInt
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait BlockStoreTest
    extends FlatSpecLike
    with Matchers
    with OptionValues
    with EitherValues
    with GeneratorDrivenPropertyChecks
    with BeforeAndAfterAll {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(100))

  private[this] def toBlockMessage(bh: String, v: Long, ts: Long): BlockMessage =
    BlockMessage(blockHash = bh)
      .withHeader(Header().withVersion(v).withTimestamp(ts))

  private[this] implicit def liftToBlockHash(s: String): BlockHash = ByteString.copyFromUtf8(s)
  private[this] implicit def liftToBlockStoreElement(
      s: (String, BlockMessage)
  ): (BlockHash, BlockMessage) =
    (ByteString.copyFromUtf8(s._1), s._2)

  def withStore[R](f: BlockStore[Task] => Task[R]): R

  "Block Store" should "return Some(message) on get for a published key" in {
    forAll(blockHashElementsGen, minSize(0), sizeRange(10)) { blockStoreElements =>
      withStore { store =>
        val items = blockStoreElements
        for {
          _ <- items.traverse_(store.put(_))
          _ <- items.traverse[Task, Assertion] {
                case (k, v) =>
                  store.get(k).map(_ shouldBe Some(v))
              }
          result <- store.asMap().map(_.size shouldEqual items.size)
          _      <- store.clear()
        } yield result
      }
    }
  }

  it should "discover keys by predicate" in {
    forAll(blockHashElementsGen, minSize(0), sizeRange(10)) { blockStoreElements =>
      withStore { store =>
        val items = blockStoreElements
        for {
          _ <- items.traverse_(store.put(_))
          _ <- items.traverse[Task, Assertion] {
                case (k, v) =>
                  store.find(_ == ByteString.copyFrom(k.getBytes())).map { w =>
                    w should have size 1
                    w.head._2 shouldBe v
                  }
              }
          result <- store.asMap().map(_.size shouldEqual items.size)
          _      <- store.clear()
        } yield result
      }
    }
  }

  it should "overwrite existing value" in
    forAll(blockHashElementsGen, minSize(0), sizeRange(10)) { blockStoreElements =>
      withStore { store =>
        val items = blockStoreElements.map {
          case (hash, elem) =>
            (hash, elem, toBlockMessage(hash, 200L, 20000L))
        }
        for {
          _ <- items.traverse_[Task, Unit] { case (k, v1, _) => store.put(k, v1) }
          _ <- items.traverse_[Task, Assertion] {
                case (k, v1, _) => store.get(k).map(_ shouldBe Some(v1))
              }
          _ <- items.traverse_[Task, Unit] { case (k, _, v2) => store.put(k, v2) }
          _ <- items.traverse_[Task, Assertion] {
                case (k, _, v2) => store.get(k).map(_ shouldBe Some(v2))
              }
          result <- store.asMap().map(_.size shouldEqual items.size)
          _      <- store.clear()
        } yield result
      }
    }

  it should "rollback the transaction on error" in {
    withStore { store =>
      val exception = new RuntimeException("msg")

      def elem = {
        blockHashElementsGen.sample.get
        throw exception
      }

      for {
        _          <- store.asMap().map(_.size shouldEqual 0)
        putAttempt <- store.put { elem }.attempt
        _          = putAttempt.left.value shouldBe exception
        result     <- store.asMap().map(_.size shouldEqual 0)
      } yield result
    }
  }
}

class InMemBlockStoreTest extends BlockStoreTest {
  override def withStore[R](f: BlockStore[Task] => Task[R]): R = {
    val test = for {
      refTask <- emptyMapRef[Task]
      metrics = new MetricsNOP[Task]()
      store   = InMemBlockStore.create[Task](Monad[Task], refTask, metrics)
      _       <- store.asMap().map(map => assert(map.isEmpty))
      result  <- f(store)
    } yield result
    test.unsafeRunSync
  }
}

class LMDBBlockStoreTest extends BlockStoreTest {

  import java.nio.file.{Files, Path}

  private[this] def mkTmpDir(): Path = Files.createTempDirectory("block-store-test-")
  private[this] val mapSize: Long    = 100L * 1024L * 1024L * 4096L

  override def withStore[R](f: BlockStore[Task] => Task[R]): R = {
    val dbDir                           = mkTmpDir()
    val env                             = Context.env(dbDir, mapSize)
    implicit val metrics: Metrics[Task] = new MetricsNOP[Task]()
    val store                           = LMDBBlockStore.create[Task](env, dbDir)
    val test = for {
      _      <- store.asMap().map(map => assert(map.isEmpty))
      result <- f(store)
    } yield result
    try {
      test.unsafeRunSync
    } finally {
      env.close()
      dbDir.recursivelyDelete()
    }
  }
}

class FileLMDBIndexBlockStoreTest extends BlockStoreTest {

  import java.nio.file.{Files, Path}

  private[this] def mkTmpDir(): Path = Files.createTempDirectory("block-store-test-")
  private[this] val mapSize: Long    = 100L * 1024L * 1024L * 4096L

  override def withStore[R](f: BlockStore[Task] => Task[R]): R = {
    val dbDir                           = mkTmpDir()
    val env                             = Context.env(dbDir, mapSize)
    implicit val metrics: Metrics[Task] = new MetricsNOP[Task]()
    val test = for {
      store <- FileLMDBIndexBlockStore.create[Task](
                FileLMDBIndexBlockStore.Config(
                  dbDir.resolve("block-store-data"),
                  dbDir.resolve("block-store-index"),
                  mapSize
                )
              )
      _      <- store.asMap().map(map => assert(map.isEmpty))
      result <- f(store)
    } yield result
    try {
      test.unsafeRunSync
    } finally {
      env.close()
      dbDir.recursivelyDelete()
    }
  }
}
