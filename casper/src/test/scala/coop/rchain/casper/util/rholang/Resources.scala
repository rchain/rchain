package coop.rchain.casper.util.rholang

import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Path}

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Resource, Sync}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.casperbuffer.{CasperBufferKeyValueStorage, CasperBufferStorage}
import coop.rchain.blockstorage.dag.{BlockDagKeyValueStorage, BlockDagStorage}
import coop.rchain.blockstorage.deploy.{DeployStorage, LMDBDeployStorage}
import coop.rchain.casper.helper.BlockDagStorageTestFixture
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources.{mkRuntimeAt, mkTempDir}
import coop.rchain.rholang.interpreter.Runtime.RhoHistoryRepository
import coop.rchain.shared.Log
import coop.rchain.store.LmdbDirStoreManager.gb
import monix.eval.Task
import monix.execution.Scheduler

object Resources {

  def mkRuntimeManager(
      prefix: String,
      storageSize: Long = 1024 * 1024L
  )(implicit scheduler: Scheduler): Resource[Task, RuntimeManager[Task]] =
    mkTempDir[Task](prefix) >>= (mkRuntimeManagerAt(_)(storageSize))

  def mkRuntimeManagerAt[F[_]: Concurrent: Parallel: ContextShift](storageDirectory: Path)(
      storageSize: Long = 1024 * 1024 * 1024L
  )(
      implicit scheduler: Scheduler
  ): Resource[F, RuntimeManager[F]] = {
    implicit val log               = Log.log[F]
    implicit val metricsEff        = new metrics.Metrics.MetricsNOP[F]
    implicit val noopSpan: Span[F] = NoopSpan[F]()

    for {
      runtime        <- mkRuntimeAt[F](storageDirectory)(storageSize)
      runtimeManager <- Resource.liftF(RuntimeManager.fromRuntime(runtime._1))
    } yield runtimeManager
  }

  def mkRuntimeManagerWithHistoryAt[F[_]: Concurrent: Parallel: ContextShift](
      storageDirectory: Path
  )(
      storageSize: Long = 1024 * 1024 * 1024L
  )(
      implicit scheduler: Scheduler
  ): Resource[F, (RuntimeManager[F], RhoHistoryRepository[F])] = {
    implicit val log               = Log.log[F]
    implicit val metricsEff        = new metrics.Metrics.MetricsNOP[F]
    implicit val noopSpan: Span[F] = NoopSpan[F]()

    for {
      runtimes              <- mkRuntimeAt[F](storageDirectory)(storageSize)
      (runtime, history, _) = runtimes
      runtimeManager        <- Resource.liftF(RuntimeManager.fromRuntime(runtime))
    } yield (runtimeManager, history)
  }

  def mkBlockStoreAt[F[_]: Concurrent: Metrics: Sync: Log](path: Path): Resource[F, BlockStore[F]] =
    Resource.make(
      BlockDagStorageTestFixture.createBlockStorage[F](path)
    )(_.close())

  def mkBlockDagStorageAt[F[_]: Concurrent: Sync: Log: Metrics](
      path: Path
  ): Resource[F, BlockDagStorage[F]] =
    Resource.liftF(for {
      storeManager <- RNodeKeyValueStoreManager[F](path, 1 * gb)
      blockDagStorage <- {
        implicit val kvm = storeManager
        BlockDagKeyValueStorage.create[F]
      }
    } yield blockDagStorage)

  def mkCasperBuferStorate[F[_]: Concurrent: Log: Metrics](
      path: Path
  ): Resource[F, CasperBufferStorage[F]] =
    Resource.liftF(for {
      storeManager <- RNodeKeyValueStoreManager[F](path, 1 * gb)
      casperBufferStorage <- {
        implicit val kvm = storeManager
        CasperBufferKeyValueStorage.create[F]
      }
    } yield casperBufferStorage)

  def mkDeployStorageAt[F[_]: Sync](path: Path): Resource[F, DeployStorage[F]] =
    LMDBDeployStorage.make[F](LMDBDeployStorage.Config(path, BlockDagStorageTestFixture.mapSize))

  case class StoragePaths(
      blockStoreDir: Path,
      blockDagDir: Path,
      lastFinalizedFile: Path,
      rspaceDir: Path,
      deployStorageDir: Path
  )

  def copyStorage[F[_]: Sync](
      storageTemplatePath: Path
  ): Resource[F, StoragePaths] =
    for {
      storageDirectory  <- mkTempDir(s"casper-test-")
      _                 <- Resource.liftF(copyDir(storageTemplatePath, storageDirectory))
      blockStoreDir     = storageDirectory.resolve("block-store")
      blockDagDir       = storageDirectory.resolve("block-dag-store")
      lastFinalizedFile = storageDirectory.resolve("last-finalized-blockhash")
      rspaceDir         = storageDirectory.resolve("rspace")
      deployStorageDir  = storageDirectory.resolve("deploy-storage")
    } yield StoragePaths(
      blockStoreDir = blockStoreDir,
      blockDagDir = blockDagDir,
      lastFinalizedFile = lastFinalizedFile,
      rspaceDir = rspaceDir,
      deployStorageDir = deployStorageDir
    )

  private def copyDir[F[_]: Sync](src: Path, dest: Path): F[Unit] = Sync[F].delay {
    Files
      .walk(src)
      .forEach(source => Files.copy(source, dest.resolve(src.relativize(source)), REPLACE_EXISTING))
  }
}
