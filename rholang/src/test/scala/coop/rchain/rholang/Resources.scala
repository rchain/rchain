package coop.rchain.rholang

import cats.Parallel
import cats.effect.ExitCase.Error
import cats.effect.{Concurrent, ContextShift, Resource, Sync}
import cats.syntax.all._
import com.typesafe.scalalogging.Logger
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.RhoRuntime.{RhoHistoryRepository, RhoISpace}
import coop.rchain.rholang.interpreter.SystemProcesses.Definition
import coop.rchain.rholang.interpreter.{ReplayRhoRuntime, RhoRuntime, RholangCLI}
import coop.rchain.rspace
import coop.rchain.rspace.RSpace.RSpaceStore
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import coop.rchain.rspace.{Match, RSpace}
import coop.rchain.shared.Log
import coop.rchain.store.KeyValueStoreManager
import monix.execution.Scheduler

import java.io.File
import java.nio.file.{Files, Path}
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

  def mkRhoISpace[F[_]: Concurrent: Parallel: ContextShift: KeyValueStoreManager: Metrics: Span: Log](
      implicit scheduler: Scheduler
  ): F[RhoISpace[F]] = {
    import coop.rchain.rholang.interpreter.storage._

    implicit val m: rspace.Match[F, BindPattern, ListParWithRandom] = matchListPar[F]

    for {
      store <- KeyValueStoreManager[F].rSpaceStores
      space <- RSpace.create[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](store)
    } yield space
  }

  def mkRuntime[F[_]: Concurrent: Parallel: ContextShift: Metrics: Span: Log](
      prefix: String
  )(implicit scheduler: Scheduler): Resource[F, RhoRuntime[F]] =
    mkTempDir(prefix)
      .evalMap(RholangCLI.mkRSpaceStoreManager[F](_))
      .evalMap(_.rSpaceStores)
      .evalMap(RhoRuntime.createRuntime(_, Par()))

  def mkRuntimes[F[_]: Concurrent: Parallel: ContextShift: Metrics: Span: Log](
      prefix: String,
      initRegistry: Boolean = false
  )(
      implicit scheduler: Scheduler
  ): Resource[F, (RhoRuntime[F], ReplayRhoRuntime[F], RhoHistoryRepository[F])] =
    mkTempDir(prefix)
      .evalMap(RholangCLI.mkRSpaceStoreManager[F](_))
      .evalMap(_.rSpaceStores)
      .evalMap(createRuntimes(_, initRegistry = initRegistry))

  def createRuntimes[F[_]: Concurrent: ContextShift: Parallel: Log: Metrics: Span](
      stores: RSpaceStore[F],
      initRegistry: Boolean = false,
      additionalSystemProcesses: Seq[Definition[F]] = Seq.empty
  )(
      implicit scheduler: Scheduler
  ): F[(RhoRuntime[F], ReplayRhoRuntime[F], RhoHistoryRepository[F])] = {
    import coop.rchain.rholang.interpreter.storage._
    implicit val m: Match[F, BindPattern, ListParWithRandom] = matchListPar[F]
    for {
      hrstores <- RSpace
                   .createWithReplay[F, Par, BindPattern, ListParWithRandom, TaggedContinuation](
                     stores
                   )
      (space, replay) = hrstores
      runtimes <- RhoRuntime
                   .createRuntimes[F](space, replay, initRegistry, additionalSystemProcesses, Par())
      (runtime, replayRuntime) = runtimes
    } yield (runtime, replayRuntime, space.historyRepo)
  }

}
