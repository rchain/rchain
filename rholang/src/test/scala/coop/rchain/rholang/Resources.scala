package coop.rchain.rholang
import java.io.File
import java.nio.file.{Files, Path}

import cats.effect.ExitCase.Error
import cats.effect.{Concurrent, ContextShift, Resource, Sync}
import cats.implicits._
import cats.temp.par
import com.typesafe.scalalogging.Logger
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.Runtime.{RhoHistoryRepository, RhoISpace, SystemProcess}
import coop.rchain.rspace
import coop.rchain.rspace.RSpace
import coop.rchain.rspace.history.{Branch, HistoryRepository}
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

  def mkRhoISpace[F[_]: Concurrent: ContextShift: par.Par: Log: Metrics: Span](
      prefix: String = ""
  ): Resource[F, RhoISpace[F]] = {

    val branch: String = "test"
    val mapSize: Long  = 1024L * 1024L * 4

    import coop.rchain.rholang.interpreter.storage._
    implicit val m: rspace.Match[F, BindPattern, ListParWithRandom] = matchListPar[F]
    import scala.concurrent.ExecutionContext.Implicits.global

    def mkRspace(dbDir: Path): F[RhoISpace[F]] =
      RSpace.create[
        F,
        Par,
        BindPattern,
        ListParWithRandom,
        TaggedContinuation
      ](dbDir, mapSize, Branch(branch))

    mkTempDir(prefix)(implicitly[Concurrent[F]])
      .flatMap(tmpDir => Resource.make(mkRspace(tmpDir))(_.close()))
  }

  def mkRuntime[F[_]: Log: Metrics: Span: Concurrent: par.Par: ContextShift](
      prefix: String,
      storageSize: Long = 1024 * 1024,
      additionalSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  )(implicit scheduler: Scheduler): Resource[F, Runtime[F]] =
    mkRuntimeWithHistory(prefix, storageSize, additionalSystemProcesses).map(_._1)

  def mkRuntimeWithHistory[F[_]: Log: Metrics: Span: Concurrent: par.Par: ContextShift](
      prefix: String,
      storageSize: Long,
      additionalSystemProcesses: Seq[SystemProcess.Definition[F]]
  )(implicit scheduler: Scheduler): Resource[F, (Runtime[F], RhoHistoryRepository[F])] =
    mkTempDir[F](prefix) >>= (mkRuntimeAt(_)(storageSize, additionalSystemProcesses))

  def mkRuntimeAt[F[_]: Log: Metrics: Span: Concurrent: par.Par: ContextShift](path: Path)(
      storageSize: Long = 1024 * 1024,
      additionalSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  )(implicit scheduler: Scheduler): Resource[F, (Runtime[F], RhoHistoryRepository[F])] =
    Resource.make[F, (Runtime[F], RhoHistoryRepository[F])](
      Runtime.setupRSpace[F](path, storageSize) >>= {
        case (space, replay, hr) =>
          Runtime
            .createWithEmptyCost[F](
              (space, replay),
              additionalSystemProcesses
            )
            .map((_, hr))

      }
    )(_._1.close())

}
