package coop.rchain.casper.helper

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import cats.Id
import coop.rchain.blockstorage.{BlockStore, LMDBBlockStore}
import coop.rchain.casper.helper.BlockGenerator.{storeForStateWithChain, StateWithChain}
import org.lmdbjava.{Env, EnvFlags}
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Suite}
import coop.rchain.shared.PathOps.RichPath

trait WithBlockStore extends BeforeAndAfter { self: Suite =>
  def withStore[R](f: BlockStore[Id] => R): R = {
    val dir   = BlockStoreFixture.dbDir
    val store = BlockStoreFixture.create(dir)
    try {
      f(store)
    } finally {
      store.close()
      dir.recursivelyDelete()
    }
  }
}

object BlockStoreFixture {
  def env(path: Path,
          mapSize: Long,
          flags: List[EnvFlags] = List(EnvFlags.MDB_NOTLS)): Env[ByteBuffer] =
    Env
      .create()
      .setMapSize(mapSize)
      .setMaxDbs(8)
      .setMaxReaders(126)
      .open(path.toFile, flags: _*)

  def dbDir: Path   = Files.createTempDirectory("casper-block-store-test-")
  val mapSize: Long = 1024L * 1024L * 4096L

  def create(dir: Path): BlockStore[Id] = {
    val environment = env(dir, mapSize)
    val blockStore  = LMDBBlockStore.createWithId(environment, dir)
    blockStore
  }
}

trait BlockStoreFixture extends BeforeAndAfterAll { self: Suite =>

  val dir = BlockStoreFixture.dbDir

  val store = BlockStoreFixture.create(dir)

  implicit val blockStore = store

  implicit val blockStoreChain = storeForStateWithChain[StateWithChain](blockStore)

  override def afterAll(): Unit = {
    store.close()
    dir.recursivelyDelete()
  }
}
