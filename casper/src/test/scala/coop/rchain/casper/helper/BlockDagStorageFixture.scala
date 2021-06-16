package coop.rchain.casper.helper

import cats.effect.{Blocker, Concurrent, ContextShift}
import cats.syntax.all._
import coop.rchain.blockstorage.{KeyValueBlockStore, _}
import coop.rchain.blockstorage.dag.{
  BlockDagKeyValueStorage,
  BlockDagStorage,
  IndexedBlockDagStorage
}
import coop.rchain.casper.util.GenesisBuilder.GenesisContext
import coop.rchain.casper.util.rholang.{Resources, RuntimeManager}
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.rholang
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{BeforeAndAfter, Suite}

import java.nio.file.{Files, Path}
import coop.rchain.casper.storage.RNodeKeyValueStoreManager

trait BlockDagStorageFixture extends BeforeAndAfter { self: Suite =>
  val scheduler = Scheduler.fixedPool("block-dag-storage-fixture-scheduler", 4)

  def withGenesis[R](
      context: GenesisContext
  )(f: BlockStore[Task] => IndexedBlockDagStorage[Task] => RuntimeManager[Task] => Task[R]): R = {
    implicit val s       = scheduler
    implicit val metrics = new MetricsNOP[Task]()
    implicit val log     = Log.log[Task]

    def create(dir: Path) =
      for {
        kvm        <- Resources.mkTestRNodeStoreManager[Task](dir)
        blocks     <- KeyValueBlockStore[Task](kvm)
        dag        <- BlockDagKeyValueStorage.create[Task](kvm)
        indexedDag <- IndexedBlockDagStorage.create[Task](dag)
        runtime    <- Resources.mkRuntimeManagerAt[Task](kvm)
      } yield (blocks, indexedDag, runtime)

    Resources
      .copyStorage[Task](context.storageDirectory)
      .evalMap(r => create(r.storageDir))
      .use(Function.uncurried(f).tupled)
      .unsafeRunSync
  }

  def withStorage[R](f: BlockStore[Task] => IndexedBlockDagStorage[Task] => Task[R]): R = {
    implicit val s       = scheduler
    implicit val metrics = new MetricsNOP[Task]()
    implicit val log     = Log.log[Task]

    BlockDagStorageTestFixture.withStorageF[Task, R](f).unsafeRunSync
  }
}

object BlockDagStorageTestFixture {
  val ioScheduler = Scheduler.io()
  val blocker     = Blocker.liftExecutionContext(ioScheduler)

  def blockDagStorageDir: Path = Files.createTempDirectory("casper-block-dag-storage-test-")
  def blockStorageDir: Path    = Files.createTempDirectory("casper-block-storage-test-")

  val mapSize: Long = 1024L * 1024L * 1024L
  def createBlockStorage[F[_]: Concurrent: ContextShift: Log](
      blockStorageDir: Path
  ): F[BlockStore[F]] =
    for {
      storeManager <- RNodeKeyValueStoreManager[F](blockStorageDir, blocker = blocker)
      blockStore   <- KeyValueBlockStore[F](storeManager)
    } yield blockStore

  def createBlockDagStorage[F[_]: Concurrent: ContextShift](blockDagStorageDir: Path)(
      implicit log: Log[F],
      metrics: Metrics[F]
  ): F[BlockDagStorage[F]] =
    for {
      storeManager    <- RNodeKeyValueStoreManager[F](blockDagStorageDir, blocker = blocker)
      blockDagStorage <- BlockDagKeyValueStorage.create[F](storeManager)
    } yield blockDagStorage

  def withStorageF[F[_]: Concurrent: ContextShift: Metrics: Log, R](
      f: BlockStore[F] => IndexedBlockDagStorage[F] => F[R]
  ): F[R] = {
    def create(dir: Path) =
      for {
        kvm        <- Resources.mkTestRNodeStoreManager[F](dir)
        blocks     <- KeyValueBlockStore[F](kvm)
        dag        <- BlockDagKeyValueStorage.create[F](kvm)
        indexedDag <- IndexedBlockDagStorage.create[F](dag)
      } yield (blocks, indexedDag)

    rholang.Resources
      .mkTempDir[F]("casper-block-dag-storage-test-")
      .evalMap(create)
      .use(Function.uncurried(f).tupled)
  }
}
