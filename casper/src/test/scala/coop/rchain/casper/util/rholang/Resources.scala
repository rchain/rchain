package coop.rchain.casper.util.rholang

import cats.effect.{Concurrent, ContextShift, Resource, Sync}
import cats.syntax.all._
import cats.{Applicative, Parallel}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.casperbuffer.{CasperBufferKeyValueStorage, CasperBufferStorage}
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.{
  BlockDagKeyValueStorage,
  BlockDagRepresentation,
  BlockDagStorage
}
import coop.rchain.blockstorage.deploy.{DeployStorage, LMDBDeployStorage}
import coop.rchain.casper.storage.RNodeKeyValueStoreManager
import coop.rchain.casper.storage.RNodeKeyValueStoreManager.rnodeDbMapping
import coop.rchain.casper.{CasperShardConf, CasperSnapshot, OnChainCasperState}
import coop.rchain.metrics
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.rholang.Resources.{mkRuntimeAt, mkTempDir}
import coop.rchain.rholang.interpreter.RhoRuntime.RhoHistoryRepository
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
      rnodeDbMapping().map {
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
      prefix: String
  )(implicit scheduler: Scheduler): Resource[F, RuntimeManager[F]] =
    mkTempDir[F](prefix).evalMap(mkTestRNodeStoreManager[F]).evalMap(mkRuntimeManagerAt[F])

  def mkRuntimeManagerAt[F[_]: Concurrent: Parallel: ContextShift](kvm: KeyValueStoreManager[F])(
      implicit scheduler: Scheduler
  ): F[RuntimeManager[F]] = {
    implicit val log               = Log.log[F]
    implicit val metricsEff        = new metrics.Metrics.MetricsNOP[F]
    implicit val noopSpan: Span[F] = NoopSpan[F]()

    for {
      runtime        <- mkRuntimeAt[F](kvm)
      runtimeManager <- RuntimeManager.fromRuntimes(runtime._1, runtime._2, runtime._3)
    } yield runtimeManager
  }

  def mkRuntimeManagerWithHistoryAt[F[_]: Concurrent: Parallel: ContextShift](
      kvm: KeyValueStoreManager[F]
  )(
      implicit scheduler: Scheduler
  ): F[(RuntimeManager[F], RhoHistoryRepository[F])] = {
    implicit val log               = Log.log[F]
    implicit val metricsEff        = new metrics.Metrics.MetricsNOP[F]
    implicit val noopSpan: Span[F] = NoopSpan[F]()

    for {
      runtimes                          <- mkRuntimeAt[F](kvm)
      (runtime, replayRuntime, history) = runtimes
      runtimeManager                    <- RuntimeManager.fromRuntimes(runtime, replayRuntime, history)
    } yield (runtimeManager, history)
  }

  def mkBlockDagStorageAt[F[_]: Concurrent: Sync: Log: Metrics](
      path: Path
  ): Resource[F, BlockDagStorage[F]] =
    Resource.liftF(for {
      storeManager    <- RNodeKeyValueStoreManager[F](path)
      blockDagStorage <- BlockDagKeyValueStorage.create[F](storeManager)
    } yield blockDagStorage)

  def mkCasperBufferStorage[F[_]: Concurrent: Log: Metrics](
      kvm: KeyValueStoreManager[F]
  ): F[CasperBufferStorage[F]] = CasperBufferKeyValueStorage.create[F](kvm)

  def mkDeployStorageAt[F[_]: Sync](path: Path): Resource[F, DeployStorage[F]] =
    LMDBDeployStorage.make[F](LMDBDeployStorage.Config(path, mapSize = 100 * mb))

  case class StoragePaths(
      storageDir: Path,
      deployStorageDir: Path
  )

  def copyStorage[F[_]: Sync](
      storageTemplatePath: Path
  ): Resource[F, StoragePaths] =
    for {
      storageDirectory <- mkTempDir(s"casper-test-")
      _                <- Resource.liftF(copyDir(storageTemplatePath, storageDirectory))
      deployStorageDir = storageDirectory.resolve("deploy-storage")
    } yield StoragePaths(
      storageDir = storageDirectory,
      deployStorageDir = deployStorageDir
    )

  private def copyDir[F[_]: Sync](src: Path, dest: Path): F[Unit] = Sync[F].delay {
    Files
      .walk(src)
      .forEach(source => Files.copy(source, dest.resolve(src.relativize(source)), REPLACE_EXISTING))
  }

  def mkDummyCasperSnapshot[F[_]: Applicative]: F[CasperSnapshot[F]] = {
    val dummyRepresentation = new BlockDagRepresentation[F] {
      override def lookup(blockHash: BlockHash): F[Option[BlockMetadata]] = ???

      override def contains(blockHash: BlockHash): F[Boolean] = ???

      override def latestMessageHash(validator: Validator): F[Option[BlockHash]] = ???

      override def latestMessageHashes: F[Map[Validator, BlockHash]] = ???

      override def invalidBlocks: F[Set[BlockMetadata]] = ???

      override def latestBlockNumber: F[Long] = ???

      override def lookupByDeployId(deployId: DeployId): F[Option[BlockHash]] = ???

      override def topoSort(
          startBlockNumber: Long,
          maybeEndBlockNumber: Option[Long]
      ): F[Vector[Vector[BlockHash]]] = ???

      override def isFinalized(blockHash: BlockHash): F[Boolean] = ???

      override def children(vertex: BlockHash): F[Option[Set[BlockHash]]] = ???

      override def parents(vertex: BlockHash): F[Option[Seq[BlockHash]]] = ???
    }
    CasperSnapshot[F](
      dummyRepresentation,
      ByteString.EMPTY,
      ByteString.EMPTY,
      IndexedSeq.empty,
      List.empty,
      Set.empty,
      Map.empty,
      Set.empty,
      0,
      Map.empty,
      OnChainCasperState(
        CasperShardConf(0, "", "", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        Map.empty,
        Seq.empty
      )
    )
  }.pure[F]

}
