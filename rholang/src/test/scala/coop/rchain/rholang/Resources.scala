package coop.rchain.rholang
import java.io.File
import java.nio.file.{Files, Path}

import cats.effect.ExitCase.Error
import cats.effect.{Concurrent, ContextShift, Resource}
import cats.{Applicative, Parallel}
import com.typesafe.scalalogging.Logger
import coop.rchain.metrics.Metrics
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.Runtime.{RhoISpace, SystemProcess}
import coop.rchain.rspace.{RSpace}
import coop.rchain.rspace.history.Branch
import coop.rchain.shared.Log
import monix.execution.Scheduler

import scala.reflect.io.Directory

object Resources {
  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  def mkTempDir[F[_]: Applicative](prefix: String): Resource[F, Path] =
    Resource.makeCase(Applicative[F].pure(Files.createTempDirectory(prefix)))(
      (path, exitCase) =>
        Applicative[F].pure(exitCase match {
          case Error(ex) =>
            logger
              .error(
                s"Exception thrown while using the tempDir '$path'. Temporary dir NOT deleted.",
                ex
              )
          case _ => new Directory(new File(path.toString)).deleteRecursively()
        })
    )

  def mkRhoISpace[F[_]: Concurrent: ContextShift: Log: Metrics](
      prefix: String = "",
      branch: String = "test",
      mapSize: Long = 1024L * 1024L * 4
  ): Resource[F, RhoISpace[F]] = {

    import coop.rchain.rholang.interpreter.storage.implicits._

    import scala.concurrent.ExecutionContext.Implicits.global

    def mkRspace(dbDir: Path): F[RhoISpace[F]] =
      RSpace.create[
        F,
        Par,
        BindPattern,
        ListParWithRandom,
        ListParWithRandom,
        TaggedContinuation
      ](dbDir, mapSize, Branch(branch))

    mkTempDir(prefix)(implicitly[Concurrent[F]])
      .flatMap(tmpDir => Resource.make(mkRspace(tmpDir))(_.close()))
  }

  def mkRuntime[F[_]]: MkRuntimePartiallyApplied[F] = new MkRuntimePartiallyApplied[F]

  /**
    * This is so that we can write {{{mkRuntime[Task]}}} instead of {{{mkRuntime[Task, Task.Par]}}}
    *
    * See https://typelevel.org/cats/guidelines.html#a-idpartially-applied-type-params-hrefpartially-applied-type-paramsa-partially-applied-type
    */
  private[rholang] final class MkRuntimePartiallyApplied[F[_]](val dummy: Boolean = true)
      extends AnyVal {

    def apply[M[_]: Parallel[F, ?[_]]](
        prefix: String,
        storageSize: Long = 1024 * 1024,
        additionalSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
    )(
        implicit scheduler: Scheduler,
        cs: ContextShift[F],
        c: Concurrent[F],
        l: Log[F],
        m: Metrics[F]
    ): Resource[F, Runtime[F]] =
      mkTempDir[F](prefix).flatMap { tmpDir =>
        Resource.make[F, Runtime[F]](
          Runtime
            .createWithEmptyCost[F, M](
              tmpDir,
              storageSize,
              additionalSystemProcesses
            )
        )(_.close())
      }
  }

}
