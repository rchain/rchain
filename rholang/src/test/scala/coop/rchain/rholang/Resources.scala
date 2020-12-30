package coop.rchain.rholang
import java.io.File
import java.nio.file.{Files, Path}

import cats.Parallel
import cats.effect.ExitCase.Error
import cats.effect.{Concurrent, ContextShift, Resource, Sync}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.{ReplayRhoRuntime, RhoRuntime}
import coop.rchain.rholang.interpreter.RhoRuntime.{RhoHistoryRepository, RhoISpace}
import coop.rchain.rholang.interpreter.SystemProcesses.Definition
import coop.rchain.rspace
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.rspace.RSpace.setUp
import coop.rchain.rspace.history.HistoryRepository
import coop.rchain.rspace.storage.RSpaceKeyValueStoreManager
import coop.rchain.shared.Log
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
  ): Resource[F, RhoISpace[F]] = {

    import coop.rchain.rholang.interpreter.storage._
    implicit val m: rspace.Match[F, BindPattern, ListParWithRandom] = matchListPar[F]
    import scala.concurrent.ExecutionContext.Implicits.global

    def mkRspace(dbDir: Path): F[RhoISpace[F]] =
      for {
        kvm <- RSpaceKeyValueStoreManager[F](dbDir)
        space <- RSpace.create[
                  F,
                  Par,
                  BindPattern,
                  ListParWithRandom,
                  TaggedContinuation
                ](kvm)
      } yield space

    mkTempDir(prefix)(implicitly[Concurrent[F]])
      .flatMap(tmpDir => Resource.make(mkRspace(tmpDir))(_.close()))
  }

  def mkRuntime[F[_]: Log: Metrics: Span: Concurrent: Parallel: ContextShift](
      prefix: String,
      storageSize: Long = 1024 * 1024 * 1024L,
      additionalSystemProcesses: Seq[Definition[F]] = Seq.empty
  )(implicit scheduler: Scheduler): Resource[F, RhoRuntime[F]] =
    mkRuntimeWithHistory(prefix, storageSize, additionalSystemProcesses).map(_._1)

  def mkRuntimeWithHistory[F[_]: Log: Metrics: Span: Concurrent: Parallel: ContextShift](
      prefix: String,
      storageSize: Long,
      additionalSystemProcesses: Seq[Definition[F]]
  )(implicit scheduler: Scheduler): Resource[F, (RhoRuntime[F], RhoHistoryRepository[F])] =
    mkTempDir[F](prefix) >>= (mkRuntimeAt(_)(storageSize, additionalSystemProcesses))

  def mkRuntimeAt[F[_]: Log: Metrics: Span: Concurrent: Parallel: ContextShift](path: Path)(
      storageSize: Long = 1024 * 1024 * 1024L,
      additionalSystemProcesses: Seq[Definition[F]] = Seq.empty,
      initRegistry: Boolean = true
  )(implicit scheduler: Scheduler): Resource[F, (RhoRuntime[F], RhoHistoryRepository[F])] = {
    import coop.rchain.rholang.interpreter.storage._

    Resource.make[F, (RhoRuntime[F], RhoHistoryRepository[F])](
      for {
        kvm           <- RSpaceKeyValueStoreManager[F](path)
        space         <- RhoRuntime.setupRhoRSpace[F](path, storageSize, kvm)
        runtime       <- RhoRuntime.createRhoRuntime[F](space, additionalSystemProcesses, initRegistry)
        historyReader <- setUp[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](kvm)
      } yield (runtime, historyReader._1)
    )(r => r._1.close >> r._2.close)
  }

  def mkHistoryReposity[F[_]: Log: Metrics: Span: Concurrent: Parallel: ContextShift](
      path: Path,
      storageSize: Long = 1024 * 1024 * 1024L
  ): Resource[F, RhoHistoryRepository[F]] = {
    import coop.rchain.rholang.interpreter.storage._

    Resource.make[F, RhoHistoryRepository[F]](
      for {
        kvm           <- RSpaceKeyValueStoreManager[F](path)
        historyReader <- setUp[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](kvm)
      } yield historyReader._1
    )(_.close())
  }

  def mkRuntimesAt[F[_]: Log: Metrics: Span: Concurrent: Parallel: ContextShift](path: Path)(
      storageSize: Long = 1024 * 1024 * 1024L,
      additionalSystemProcesses: Seq[Definition[F]] = Seq.empty
  )(implicit scheduler: Scheduler): Resource[F, (RhoRuntime[F], ReplayRhoRuntime[F])] = {
    import coop.rchain.rholang.interpreter.storage._

    Resource.make[F, (RhoRuntime[F], ReplayRhoRuntime[F])](
      for {
        kvm         <- RSpaceKeyValueStoreManager[F](path)
        space       <- RhoRuntime.setupRhoRSpace[F](path, storageSize, kvm)
        runtime     <- RhoRuntime.createRhoRuntime[F](space, additionalSystemProcesses)
        replaySpace <- RhoRuntime.setupReplaySpace[F](path, storageSize, kvm)
        replayRuntime <- RhoRuntime
                          .createReplayRhoRuntime[F](replaySpace, additionalSystemProcesses)
      } yield (runtime, replayRuntime)
    )(r => r._1.close >> r._2.close)
  }
}
