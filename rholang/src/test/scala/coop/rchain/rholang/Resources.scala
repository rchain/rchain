package coop.rchain.rholang
import java.io.File
import java.nio.file.{Files, Path}

import cats.Parallel
import cats.effect.ExitCase.Error
import cats.effect.{Concurrent, ContextShift, Resource, Sync}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.Runtime.{RhoHistoryRepository, RhoISpace, SystemProcess}
import coop.rchain.rspace
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.rspace.RSpace.setUp
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.storage.RSpaceKeyValueStoreManager
import coop.rchain.shared.Log
import coop.rchain.store.KeyValueStoreManager
import monix.execution.Scheduler

import scala.reflect.io.Directory

object Resources {
  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  def mkTempDir[F[_]: Sync](prefix: String): Resource[F, Path] =
    Resource.makeCase(Sync[F].delay(Files.createTempDirectory(prefix)))(
      (path, exitCase) =>
        Sync[F].delay(exitCase match {
          case Error(ex) =>
            logger
              .error(
                s"Exception thrown while using the tempDir '$path'. Temporary dir NOT deleted.",
                ex
              )
          case _ => new Directory(new File(path.toString)).deleteRecursively()
        })
    )

  def mkRhoISpace[F[_]: Concurrent: ContextShift: Parallel: Log: Metrics: Span](
      prefix: String = ""
  ): Resource[F, (RhoISpace[F], KeyValueStoreManager[F])] = {

    val mapSize: Long = 1024L * 1024L * 4

    import coop.rchain.rholang.interpreter.storage._
    implicit val m: rspace.Match[F, BindPattern, ListParWithRandom] = matchListPar[F]
    import scala.concurrent.ExecutionContext.Implicits.global

    def mkRspace(dbDir: Path): F[(RhoISpace[F], KeyValueStoreManager[F])] =
      for {
        kvm     <- RSpaceKeyValueStoreManager[F](dbDir, mapSize)
        roots   <- kvm.store("roots")
        cold    <- kvm.store("cold")
        history <- kvm.store("history")
        space <- RSpace.create[
                  F,
                  Par,
                  BindPattern,
                  ListParWithRandom,
                  TaggedContinuation
                ](roots, cold, history)
      } yield (space, kvm)

    mkTempDir(prefix)(implicitly[Concurrent[F]])
      .flatMap(tmpDir => Resource.make(mkRspace(tmpDir))(r => r._2.shutdown))
  }

  def mkRuntime[F[_]: Log: Metrics: Span: Concurrent: Parallel: ContextShift](
      prefix: String,
      storageSize: Long = 1024 * 1024 * 1024L,
      additionalSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  )(implicit scheduler: Scheduler): Resource[F, Runtime[F]] =
    mkRuntimeWithHistory(prefix, storageSize, additionalSystemProcesses).map(_._1)

  def mkRuntimeWithHistory[F[_]: Log: Metrics: Span: Concurrent: Parallel: ContextShift](
      prefix: String,
      storageSize: Long,
      additionalSystemProcesses: Seq[SystemProcess.Definition[F]]
  )(
      implicit scheduler: Scheduler
  ): Resource[F, (Runtime[F], RhoHistoryRepository[F], KeyValueStoreManager[F])] =
    mkTempDir[F](prefix) >>= (mkRuntimeAt(_)(storageSize, additionalSystemProcesses))

  def mkRuntimeAt[F[_]: Log: Metrics: Span: Concurrent: Parallel: ContextShift](path: Path)(
      storageSize: Long = 1024 * 1024 * 1024L,
      additionalSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  )(
      implicit scheduler: Scheduler
  ): Resource[F, (Runtime[F], RhoHistoryRepository[F], KeyValueStoreManager[F])] = {
    def makeRuntime =
      for {
        kvm                 <- RSpaceKeyValueStoreManager[F](path, storageSize)
        roots               <- kvm.store("roots")
        cold                <- kvm.store("cold")
        history             <- kvm.store("history")
        spaces              <- Runtime.setupRSpace[F](roots, cold, history)
        (space, replay, hr) = spaces
        runtime             <- Runtime.createWithEmptyCost[F]((space, replay), additionalSystemProcesses)
      } yield (runtime, hr, kvm)
    Resource.make[F, (Runtime[F], RhoHistoryRepository[F], KeyValueStoreManager[F])](makeRuntime)(
      _._3.shutdown
    )
  }

}
