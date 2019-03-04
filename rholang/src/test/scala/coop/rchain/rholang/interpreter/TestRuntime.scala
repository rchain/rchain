package coop.rchain.rholang.interpreter

import java.nio.file.Paths

import cats._
import cats.effect.{Concurrent, ContextShift}
import cats.implicits._
import coop.rchain.metrics.Metrics
import coop.rchain.shared.Log
import coop.rchain.shared.StoreType.InMem

import scala.concurrent.ExecutionContext

object TestRuntime {
  def create[F[_]: ContextShift: Concurrent: Log: Metrics, M[_]](
      extraSystemProcesses: Seq[Runtime.SystemProcess.Definition[F]] = Seq.empty
  )(implicit P: Parallel[F, M], executionContext: ExecutionContext): F[Runtime[F]] =
    Runtime.create[F, M](
      Paths.get("/not/a/path"),
      -1,
      InMem,
      extraSystemProcesses
    )
}
