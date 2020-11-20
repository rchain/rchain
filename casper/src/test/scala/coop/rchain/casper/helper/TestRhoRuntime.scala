package coop.rchain.casper.helper
import java.nio.file.{Files, Path}

import cats.Parallel
import cats.effect.{Concurrent, ContextShift}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rholang.Resources.mkRuntimeAt
import coop.rchain.shared.Log
import monix.execution.Scheduler.Implicits.global

object TestRhoRuntime {
  def rhoRuntimeEff[F[_]: Log: Metrics: Span: Concurrent: Parallel: ContextShift](
      initRegistry: Boolean = true
  ) = {
    val storageDirectory = Files.createTempDirectory(s"hash-set-casper-test-genesis-")
    mkRuntimeAt[F](storageDirectory)(1024L * 1024L * 1024L, initRegistry = initRegistry)
  }
}
