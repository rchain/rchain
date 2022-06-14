package coop.rchain.casper.helper

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.dag.BlockDagKeyValueStorage
import coop.rchain.casper.rholang.{Resources, RuntimeManager}
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.util.GenesisBuilder.GenesisContext
import coop.rchain.catscontrib.TaskContrib.TaskOps
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.rholang
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{BeforeAndAfter, Suite}

import java.nio.file.{Files, Path}

trait BlockDagStorageFixture extends BeforeAndAfter { self: Suite =>
  val scheduler = Scheduler.fixedPool("block-dag-storage-fixture-scheduler", 4)

  val mkCasperSnapshot = BlockGenerator.mkCasperSnapshot

  def withGenesis[R](
      context: GenesisContext
  )(f: BlockStore[Task] => BlockDagStorage[Task] => RuntimeManager[Task] => Task[R]): R = {
    implicit val s       = scheduler
    implicit val metrics = new MetricsNOP[Task]()
    implicit val log     = Log.log[Task]

    def create(dir: Path) =
      for {
        kvm        <- Resources.mkTestRNodeStoreManager[Task](dir)
        blocks     <- BlockStore[Task](kvm)
        dag        <- BlockDagKeyValueStorage.create[Task](kvm)
        indexedDag = BlockDagStorage[Task](dag)
        runtime    <- Resources.mkRuntimeManagerAt[Task](kvm)
      } yield (blocks, indexedDag, runtime)

    Resources
      .copyStorage[Task](context.storageDirectory)
      .evalMap(create)
      .use(Function.uncurried(f).tupled)
      .unsafeRunSync
  }

  def withStorage[R](f: BlockStore[Task] => BlockDagStorage[Task] => Task[R]): R = {
    implicit val s       = scheduler
    implicit val metrics = new MetricsNOP[Task]()
    implicit val log     = Log.log[Task]

    BlockDagStorageTestFixture.withStorageF[Task, R](f).unsafeRunSync
  }
}

object BlockDagStorageTestFixture {
  def blockDagStorageDir: Path = Files.createTempDirectory("casper-block-dag-storage-test-")
  def blockStorageDir: Path    = Files.createTempDirectory("casper-block-storage-test-")

  val mapSize: Long = 1024L * 1024L * 1024L
  def createBlockStorage[F[_]: Concurrent: Log](
      blockStorageDir: Path
  ): F[BlockStore[F]] =
    for {
      storeManager <- RNodeKeyValueStoreManager[F](blockStorageDir)
      blockStore   <- BlockStore[F](storeManager)
    } yield blockStore

  def createBlockDagStorage[F[_]: Concurrent](blockDagStorageDir: Path)(
      implicit log: Log[F],
      metrics: Metrics[F]
  ): F[BlockDagStorage[F]] =
    for {
      storeManager    <- RNodeKeyValueStoreManager[F](blockDagStorageDir)
      blockDagStorage <- BlockDagKeyValueStorage.create[F](storeManager)
    } yield blockDagStorage

  def withStorageF[F[_]: Concurrent: Metrics: Log, R](
      f: BlockStore[F] => BlockDagStorage[F] => F[R]
  ): F[R] = {
    def create(dir: Path) =
      for {
        kvm        <- Resources.mkTestRNodeStoreManager[F](dir)
        blocks     <- BlockStore[F](kvm)
        dag        <- BlockDagKeyValueStorage.create[F](kvm)
        indexedDag = BlockDagStorage[F](dag)
      } yield (blocks, indexedDag)

    rholang.Resources
      .mkTempDir[F]("casper-block-dag-storage-test-")
      .evalMap(create)
      .use(Function.uncurried(f).tupled)
  }
}
