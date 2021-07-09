package coop.rchain.blockstorage

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.InMemBlockStore.emptyMapRef
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.lmdb.Context
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.blockImplicits.{blockBatchesGen, blockElementsGen}
import coop.rchain.shared.Log
import coop.rchain.shared.PathOps._
import monix.eval.Task
import monix.execution.Scheduler
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

  private[this] def toBlockMessage(bh: BlockHash, v: Long, ts: Long): BlockMessage =
    BlockMessage
      .from(
        BlockMessageProto()
          .withBlockHash(bh)
          .withHeader(
            HeaderProto()
              .withVersion(v)
              .withTimestamp(ts)
          )
          .withBody(BodyProto().withState(RChainStateProto()))
      )
      .right
      .get

  def withStore[R](f: BlockStore[Task] => Task[R]): R

  "Block Store" should "return Some(message) on get for a published key" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockStoreElements =>
      withStore { store =>
        val items = blockStoreElements
        for {
          _ <- items.traverse_(store.put)
          _ <- items.traverse[Task, Assertion] { block =>
                store.get(block.blockHash).map(_ shouldBe Some(block))
              }
          result <- store.find(_ => true).map(_.size shouldEqual items.size)
        } yield result
      }
    }
  }

  it should "discover keys by predicate" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockStoreElements =>
      withStore { store =>
        val items = blockStoreElements
        for {
          _ <- items.traverse_(store.put)
          _ <- items.traverse[Task, Assertion] { block =>
                store.find(_ == ByteString.copyFrom(block.blockHash.toByteArray)).map { w =>
                  w should have size 1
                  w.head._2 shouldBe block
                }
              }
          result <- store.find(_ => true).map(_.size shouldEqual items.size)
        } yield result
      }
    }
  }

  it should "overwrite existing value" in
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockStoreElements =>
      withStore { store =>
        val items = blockStoreElements.map { block =>
          (block.blockHash, block, toBlockMessage(block.blockHash, 200L, 20000L))
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
          result <- store.find(_ => true).map(_.size shouldEqual items.size)
        } yield result
      }
    }

  it should "rollback the transaction on error" in {
    withStore { store =>
      val exception = new RuntimeException("msg")

      def elem: (BlockHash, BlockMessage) =
        throw exception

      for {
        _          <- store.find(_ => true).map(_.size shouldEqual 0)
        putAttempt <- store.put { elem }.attempt
        _          = putAttempt.left.value shouldBe exception
        result     <- store.find(_ => true).map(_.size shouldEqual 0)
      } yield result
    }
  }
}

class InMemBlockStoreTest extends BlockStoreTest {
  override def withStore[R](f: BlockStore[Task] => Task[R]): R = {
    implicit val metrics = new MetricsNOP[Task]()
    val test = for {
      refTask          <- emptyMapRef[Task]
      approvedBlockRef <- Ref[Task].of(none[ApprovedBlock])
      store            = InMemBlockStore.create[Task](refTask, approvedBlockRef)
      _                <- store.find(_ => true).map(map => assert(map.isEmpty))
      result           <- f(store)
    } yield result
    test.unsafeRunSync
  }
}

class FileLMDBIndexBlockStoreTest extends BlockStoreTest {
  val scheduler = Scheduler.fixedPool("block-storage-test-scheduler", 4)

  import java.nio.file.{Files, Path}

  private[this] def mkTmpDir(): Path = Files.createTempDirectory("block-store-test-")
  private[this] val mapSize: Long    = 100L * 1024L * 1024L * 4096L

  override def withStore[R](f: BlockStore[Task] => Task[R]): R = {
    val dbDir                   = mkTmpDir()
    implicit val log: Log[Task] = new Log.NOPLog[Task]()
    implicit val metrics        = new Metrics.MetricsNOP[Task]
    val env                     = Context.env(dbDir, mapSize)
    val test = for {
      store  <- FileLMDBIndexBlockStore.create[Task](env, dbDir).map(_.right.get)
      _      <- store.find(_ => true).map(map => assert(map.isEmpty))
      result <- f(store)
    } yield result
    try {
      test.unsafeRunSync
    } finally {
      env.close()
      dbDir.recursivelyDelete()
    }
  }

  private def createBlockStore(blockStoreDataDir: Path): Task[BlockStore[Task]] = {
    implicit val log     = new Log.NOPLog[Task]()
    implicit val metrics = new Metrics.MetricsNOP[Task]
    val env              = Context.env(blockStoreDataDir, 100L * 1024L * 1024L * 4096L)
    FileLMDBIndexBlockStore.create[Task](env, blockStoreDataDir).map(_.right.get)
  }

  def withStoreLocation[R](f: Path => Task[R]): R = {
    val testProgram = Sync[Task].bracket {
      Sync[Task].delay {
        mkTmpDir()
      }
    } { blockStoreDataDir =>
      f(blockStoreDataDir)
    } { blockStoreDataDir =>
      Sync[Task].delay {
        blockStoreDataDir.recursivelyDelete()
      }
    }
    testProgram.unsafeRunSync(scheduler)
  }

  "FileLMDBIndexBlockStore" should "persist storage on restart" in {
    forAll(blockElementsGen, minSize(0), sizeRange(10)) { blockStoreElements =>
      withStoreLocation { blockStoreDataDir =>
        for {
          firstStore  <- createBlockStore(blockStoreDataDir)
          _           <- blockStoreElements.traverse_[Task, Unit](firstStore.put)
          _           <- firstStore.close()
          secondStore <- createBlockStore(blockStoreDataDir)
          _ <- blockStoreElements.traverse[Task, Assertion] { block =>
                secondStore.get(block.blockHash).map(_ shouldBe Some(block))
              }
          result <- secondStore.find(_ => true).map(_.size shouldEqual blockStoreElements.size)
          _      <- secondStore.close()
        } yield result
      }
    }
  }

  "FileLMDBIndexBlockStore" should "persist approved block on restart" in {
    withStoreLocation { blockStoreDataDir =>
      val approvedBlock =
        ApprovedBlock
          .from(
            ApprovedBlockProto(
              Some(
                ApprovedBlockCandidateProto(
                  Some(
                    BlockMessageProto()
                      .withBody(BodyProto().withState(RChainStateProto()))
                      .withHeader(HeaderProto())
                  ),
                  1
                )
              ),
              List(Signature(ByteString.EMPTY, "", ByteString.EMPTY))
            )
          )
          .right
          .get
      for {
        firstStore          <- createBlockStore(blockStoreDataDir)
        _                   <- firstStore.putApprovedBlock(approvedBlock)
        _                   <- firstStore.close()
        secondStore         <- createBlockStore(blockStoreDataDir)
        storedApprovedBlock <- secondStore.getApprovedBlock
        _                   = storedApprovedBlock shouldBe Some(approvedBlock)
        _                   <- secondStore.close()
      } yield ()
    }
  }

  it should "persist storage after checkpoint" in {
    forAll(blockElementsGen, minSize(10), sizeRange(10)) { blockStoreElements =>
      withStoreLocation { blockStoreDataDir =>
        val (firstHalf, secondHalf) = blockStoreElements.splitAt(blockStoreElements.size / 2)
        for {
          firstStore <- createBlockStore(blockStoreDataDir)
          _          <- firstHalf.traverse_[Task, Unit](firstStore.put)
          _          <- firstStore.checkpoint()
          _          <- secondHalf.traverse_[Task, Unit](firstStore.put)
          _ <- blockStoreElements.traverse[Task, Assertion] { block =>
                firstStore.get(block.blockHash).map(_ shouldBe Some(block))
              }
          _           <- firstStore.find(_ => true).map(_.size shouldEqual blockStoreElements.size)
          _           <- firstStore.close()
          secondStore <- createBlockStore(blockStoreDataDir)
          _ <- blockStoreElements.traverse[Task, Assertion] { block =>
                secondStore.get(block.blockHash).map(_ shouldBe Some(block))
              }
          result <- secondStore.find(_ => true).map(_.size shouldEqual blockStoreElements.size)
          _      <- secondStore.close()
        } yield result
      }
    }
  }

  it should "be able to store multiple checkpoints" in {
    forAll(blockBatchesGen, minSize(5), sizeRange(10)) { blockStoreBatches =>
      withStoreLocation { blockStoreDataDir =>
        val blocks = blockStoreBatches.flatten
        for {
          firstStore <- createBlockStore(blockStoreDataDir)
          _ <- blockStoreBatches.traverse_[Task, Unit](
                blockStoreElements =>
                  blockStoreElements
                    .traverse_[Task, Unit](firstStore.put) >> firstStore.checkpoint()
              )
          _ <- blocks.traverse[Task, Assertion] { block =>
                firstStore.get(block.blockHash).map(_ shouldBe Some(block))
              }
          _           <- firstStore.find(_ => true).map(_.size shouldEqual blocks.size)
          _           <- firstStore.close()
          secondStore <- createBlockStore(blockStoreDataDir)
          _ <- blocks.traverse[Task, Assertion] { block =>
                secondStore.get(block.blockHash).map(_ shouldBe Some(block))
              }
          result <- secondStore.find(_ => true).map(_.size shouldEqual blocks.size)
          _      <- secondStore.close()
        } yield result
      }
    }
  }
}
