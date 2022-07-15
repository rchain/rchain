package coop.rchain.casper.helper

import cats.effect.{Concurrent, Resource}
import cats.syntax.all._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.BlockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.casper.dag.BlockDagKeyValueStorage
import coop.rchain.casper.rholang.{BlockRandomSeed, Resources, RuntimeManager}
import coop.rchain.casper.util.GenesisBuilder.GenesisContext
import coop.rchain.metrics.Metrics
import coop.rchain.metrics.Metrics.MetricsNOP
import coop.rchain.rholang
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfter, Suite}

import java.nio.file.Path

trait BlockDagStorageFixture extends BeforeAndAfter { self: Suite =>
  val dummyParentsPreState = BlockGenerator.dummyParentsPreState

  def withGenesis[R](
      context: GenesisContext
  )(f: BlockStore[Task] => BlockDagStorage[Task] => RuntimeManager[Task] => Task[R]): R = {
    implicit val metrics = new MetricsNOP[Task]()
    implicit val log     = Log.log[Task]

    def create(dir: Path) =
      for {
        kvm        <- Resources.mkTestRNodeStoreManager[Task](dir)
        blocks     <- BlockStore[Task](kvm)
        dag        <- BlockDagKeyValueStorage.create[Task](kvm)
        indexedDag = BlockDagStorage[Task](dag)
        runtime <- Resources.mkRuntimeManagerAt[Task](
                    kvm,
                    BlockRandomSeed.nonNegativeMergeableTagName(context.genesisBlock.shardId)
                  )
      } yield (blocks, indexedDag, runtime)

    Resources
      .copyStorage[Task](context.storageDirectory)
      .evalMap(create)
      .use(Function.uncurried(f).tupled)
      .runSyncUnsafe()
  }

  def withStorage[R](f: BlockStore[Task] => BlockDagStorage[Task] => Task[R]): R = {
    implicit val metrics = new MetricsNOP[Task]()
    implicit val log     = Log.log[Task]

    BlockDagStorageTestFixture.withStorageF[Task].use(Function.uncurried(f).tupled).runSyncUnsafe()
  }
}

object BlockDagStorageTestFixture {

  def withStorageF[F[_]: Concurrent: Metrics: Log]
      : Resource[F, (BlockStore[F], BlockDagStorage[F])] = {
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
  }
}
