package coop.rchain.casper.helper

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import cats.Id
import cats.effect.Sync
import coop.rchain.blockstorage.{BlockStore, LMDBBlockStore}
import coop.rchain.casper.helper.BlockGenerator.{storeForStateWithChain, StateWithChain}
import coop.rchain.metrics.Metrics
import org.lmdbjava.{Env, EnvFlags}
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Suite}
import coop.rchain.shared.PathOps.RichPath
import monix.eval.Task

trait BlockStoreFixture extends BeforeAndAfter { self: Suite =>
  def withStore[F[_]: Sync: Metrics, R](f: BlockStore[F] => R): R = {
    val dir   = BlockStoreTestFixture.dbDir
    val store = BlockStoreTestFixture.create[F](dir)
    try {
      f(store)
    } finally {
      store.close()
      dir.recursivelyDelete()
    }
  }
}

object BlockStoreTestFixture {
  def env(
      path: Path,
      mapSize: Long,
      flags: List[EnvFlags] = List(EnvFlags.MDB_NOTLS)
  ): Env[ByteBuffer] =
    Env
      .create()
      .setMapSize(mapSize)
      .setMaxDbs(8)
      .setMaxReaders(126)
      .open(path.toFile, flags: _*)

  def dbDir: Path   = Files.createTempDirectory("casper-block-store-test-")
  val mapSize: Long = 1024L * 1024L * 100L

  def create[F[_]: Sync: Metrics](dir: Path): BlockStore[F] = {
    val environment = env(dir, mapSize)
    val blockStore  = LMDBBlockStore.create(environment, dir)
    blockStore
  }
}

trait BlockStoreTestFixture extends BeforeAndAfterAll { self: Suite =>

  import coop.rchain.metrics.Metrics.MetricsNOP

  implicit val metrics: Metrics[Task] = new MetricsNOP[Task]

  val dir = BlockStoreTestFixture.dbDir

  val store = BlockStoreTestFixture.create[Task](dir)

  implicit val blockStore: BlockStore[Task] = store

  implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)

  override def afterAll(): Unit = {
    store.close()
    dir.recursivelyDelete()
  }
}
