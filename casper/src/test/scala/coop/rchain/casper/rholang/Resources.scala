package coop.rchain.casper.rholang

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Resource, Sync}
import cats.syntax.all._
import coop.rchain.casper.storage.RNodeKeyValueStoreManager.rnodeDbMapping
import coop.rchain.metrics
import coop.rchain.metrics.{NoopSpan, Span}
import coop.rchain.models.Par
import coop.rchain.rholang.Resources.mkTempDir
import coop.rchain.rspace.syntax._
import coop.rchain.shared.Log
import coop.rchain.store.LmdbDirStoreManager.mb
import coop.rchain.store.{KeyValueStoreManager, LmdbDirStoreManager}
import monix.execution.Scheduler

import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, Path}

object Resources {

  def mkTestRNodeStoreManager[F[_]: Concurrent: Log](
      dirPath: Path
  ): F[KeyValueStoreManager[F]] = {
    // Limit maximum environment (file) size for LMDB in tests
    val limitSize = 100 * mb
    val dbMappings = Sync[F].delay {
      rnodeDbMapping.map {
        case (db, conf) =>
          val newConf =
            if (conf.maxEnvSize > limitSize) conf.copy(maxEnvSize = limitSize)
            else conf
          (db, newConf)
      }
    }
    dbMappings >>= (xs => LmdbDirStoreManager[F](dirPath, xs.toMap))
  }

  def mkRuntimeManager[F[_]: Concurrent: Parallel: ContextShift: Log](
      prefix: String,
      mergeableTagName: Par
  )(implicit scheduler: Scheduler): Resource[F, RuntimeManager[F]] =
    mkTempDir[F](prefix)
      .evalMap(mkTestRNodeStoreManager[F])
      .evalMap(mkRuntimeManagerAt[F](_, mergeableTagName))

  // TODO: This is confusing to create another instances for Log, Metrics and Span.
  //   Investigate if it can be removed or define it as parameters. Similar for [[mkRuntimeManagerWithHistoryAt]].
  def mkRuntimeManagerAt[F[_]: Concurrent: Parallel: ContextShift](
      kvm: KeyValueStoreManager[F],
      mergeableTagName: Par
  )(
      implicit scheduler: Scheduler
  ): F[RuntimeManager[F]] = {
    implicit val log               = Log.log[F]
    implicit val metricsEff        = new metrics.Metrics.MetricsNOP[F]
    implicit val noopSpan: Span[F] = NoopSpan[F]()

    for {
      rStore <- kvm.rSpaceStores
      mStore <- RuntimeManager.mergeableStore(kvm)
      runtimeManager <- RuntimeManager(
                         rStore,
                         mStore,
                         mergeableTagName,
                         RuntimeManager.noOpExecutionTracker[F]
                       )
    } yield runtimeManager
  }

  def copyStorage[F[_]: Sync](
      storageTemplatePath: Path
  ): Resource[F, Path] =
    mkTempDir(s"casper-test-").evalTap(copyDir[F](storageTemplatePath, _))

  private def copyDir[F[_]: Sync](src: Path, dest: Path): F[Unit] = Sync[F].delay {
    Files
      .walk(src)
      .forEach(source => Files.copy(source, dest.resolve(src.relativize(source)), REPLACE_EXISTING))
  }

}
