package coop.rchain.casper.util.rholang

import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Path}

import cats.effect.{Concurrent, ContextShift, Resource, Sync}
import cats.implicits._
import cats.temp.par
import coop.rchain.blockstorage.dag.{BlockDagFileStorage, BlockDagStorage}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.helper.BlockDagStorageTestFixture
import coop.rchain.casper.helper.HashSetCasperTestNode.makeBlockDagFileStorageConfig
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources.{mkRuntimeAt, mkTempDir}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler

object Resources {

  def mkRuntimeManager(
      prefix: String,
      storageSize: Long = 1024 * 1024L
  )(implicit scheduler: Scheduler): Resource[Task, RuntimeManager[Task]] =
    mkTempDir[Task](prefix) >>= (mkRuntimeManagerAt(_)(storageSize))

  def mkRuntimeManagerAt[F[_]: Concurrent: par.Par: ContextShift](storageDirectory: Path)(
      storageSize: Long = 10 * 1024 * 1024L
  )(
      implicit scheduler: Scheduler
  ): Resource[F, RuntimeManager[F]] = {
    implicit val log               = Log.log[F]
    implicit val metricsEff        = new metrics.Metrics.MetricsNOP[F]
    implicit val noopSpan: Span[F] = NoopSpan[F]()

    for {
      runtime        <- mkRuntimeAt[F](storageDirectory)(storageSize)
      runtimeManager <- Resource.liftF(RuntimeManager.fromRuntime(runtime))
    } yield runtimeManager
  }

  def mkBlockStoreAt[F[_]: Concurrent: Metrics: Sync: Log](path: Path): Resource[F, BlockStore[F]] =
    Resource.make(
      BlockDagStorageTestFixture.createBlockStorage[F](path)
    )(_.close())

  def mkBlockDagStorageAt[F[_]: Concurrent: Sync: Log: Metrics](
      path: Path
  ): Resource[F, BlockDagStorage[F]] =
    Resource
      .make(
        BlockDagFileStorage
          .create[F](makeBlockDagFileStorageConfig(path))
          .widen
      )(_.close())
      .widen

  case class StoragePaths(
      blockStoreDir: Path,
      blockDagDir: Path,
      rspaceDir: Path
  )

  def copyStorage[F[_]: Sync](
      storageTemplatePath: Path
  ): Resource[F, StoragePaths] =
    for {
      storageDirectory <- mkTempDir(s"casper-test-")
      _                <- Resource.liftF(copyDir(storageTemplatePath, storageDirectory))
      blockStoreDir    = storageDirectory.resolve("block-store")
      blockDagDir      = storageDirectory.resolve("block-dag-store")
      rspaceDir        = storageDirectory.resolve("rspace")
    } yield StoragePaths(
      blockStoreDir = blockStoreDir,
      blockDagDir = blockDagDir,
      rspaceDir = rspaceDir
    )

  private def copyDir[F[_]: Sync](src: Path, dest: Path): F[Unit] = Sync[F].delay {
    Files
      .walk(src)
      .forEach(source => Files.copy(source, dest.resolve(src.relativize(source)), REPLACE_EXISTING))
  }
}
