package coop.rchain.rholang

import cats.Parallel
import cats.effect.ExitCase.Error
import cats.effect.{Concurrent, ContextShift, Resource, Sync}
import cats.syntax.all._
import com.typesafe.scalalogging.Logger
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.{BindPattern, ListParWithRandom, Par, TaggedContinuation}
import coop.rchain.rholang.interpreter.{ReplayRhoRuntime, RhoRuntime, RholangCLI}
import coop.rchain.rholang.interpreter.RhoRuntime.{RhoHistoryRepository, RhoISpace}
import coop.rchain.rholang.interpreter.SystemProcesses.Definition
import coop.rchain.rspace
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
      space <- RSpace.create[
                F,
                Par,
                BindPattern,
                ListParWithRandom,
                TaggedContinuation
              ](store)
    } yield space
  }

  def mkRuntime[F[_]: Concurrent: Parallel: ContextShift: Metrics: Span: Log](
      prefix: String
  )(implicit scheduler: Scheduler): Resource[F, RhoRuntime[F]] =
    mkTempDir(prefix)
      .evalMap(RholangCLI.mkRSpaceStoreManager[F](_))
      .evalMap(mkRuntimeAt(_))
      .map(_._1)

  def mkRuntimes[F[_]: Concurrent: Parallel: ContextShift: Metrics: Span: Log](
      prefix: String
  )(
      implicit scheduler: Scheduler
  ): Resource[F, (RhoRuntime[F], ReplayRhoRuntime[F], RhoHistoryRepository[F])] =
    mkTempDir(prefix)
      .evalMap(RholangCLI.mkRSpaceStoreManager[F](_))
      .evalMap(mkRuntimeAt(_))

  def mkRuntimeAt[F[_]: Concurrent: Parallel: ContextShift: Metrics: Span: Log](
      kvm: KeyValueStoreManager[F],
      additionalSystemProcesses: Seq[Definition[F]] = Seq.empty
  )(
      implicit scheduler: Scheduler
  ): F[(RhoRuntime[F], ReplayRhoRuntime[F], RhoHistoryRepository[F])] =
    for {
      store <- kvm.rSpaceStores
      runtimes <- RhoRuntime
                   .createRuntimes[F](store, additionalSystemProcesses = additionalSystemProcesses)
      (runtime, replayRuntime, hr) = runtimes
    } yield (runtime, replayRuntime, hr)

}
