package coop.rchain.casper.helper

import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.{BlockDagKeyValueStorage, IndexedBlockDagStorage}
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

import java.nio.file.Path

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
      .map(_.storageDir)
      .evalMap(create)
      .use(Function.uncurried(f).tupled)
      .unsafeRunSync
  }

  def withStorage[R](f: BlockStore[Task] => IndexedBlockDagStorage[Task] => Task[R]): R = {
    implicit val s       = scheduler
    implicit val metrics = new MetricsNOP[Task]()
    implicit val log     = Log.log[Task]

    withStorageF[Task, R](f).unsafeRunSync
  }

  def withStorageF[F[_]: Concurrent: Metrics: Log, R](
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
