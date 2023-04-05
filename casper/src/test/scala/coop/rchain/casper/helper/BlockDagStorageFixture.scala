package coop.rchain.casper.helper

import cats.effect.{Async, IO, Resource}
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
import coop.rchain.shared.{Log, Time}
import org.scalatest.{BeforeAndAfter, Suite}

import java.nio.file.Path

trait BlockDagStorageFixture extends BeforeAndAfter { self: Suite =>
  val dummyParentsPreState = BlockGenerator.dummyParentsPreState

  def withGenesis[R](
      context: GenesisContext
  )(f: BlockStore[IO] => BlockDagStorage[IO] => RuntimeManager[IO] => IO[R]): R = {
    implicit val metrics = new MetricsNOP[IO]()
    implicit val log     = Log.log[IO]
    import coop.rchain.shared.RChainScheduler._

    def create(dir: Path) =
      for {
        kvm        <- Resources.mkTestRNodeStoreManager[IO](dir)
        blocks     <- BlockStore[IO](kvm)
        dag        <- BlockDagKeyValueStorage.create[IO](kvm)
        indexedDag = BlockDagStorage[IO](dag)
        runtime <- Resources.mkRuntimeManagerAt[IO](
                    kvm,
                    BlockRandomSeed.nonNegativeMergeableTagName(context.genesisBlock.shardId)
                  )
      } yield (blocks, indexedDag, runtime)

    Resources
      .copyStorage[IO](context.storageDirectory)
      .evalMap(create)
      .use(Function.uncurried(f).tupled)
      .unsafeRunSync
  }

  def withStorage[R](f: BlockStore[IO] => BlockDagStorage[IO] => IO[R]): R = {
    implicit val metrics = new MetricsNOP[IO]()
    implicit val log     = Log.log[IO]
    import coop.rchain.shared.RChainScheduler._

    BlockDagStorageTestFixture.withStorageF[IO].use(Function.uncurried(f).tupled).unsafeRunSync
  }
}

object BlockDagStorageTestFixture {

  def withStorageF[F[_]: Async: Metrics: Log]
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
