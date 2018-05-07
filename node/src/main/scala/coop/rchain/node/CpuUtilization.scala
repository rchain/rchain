package coop.rchain.node

import cats._
import cats.implicits._

import coop.rchain.catscontrib._
import coop.rchain.p2p.effects.Metrics

trait CpuUtilization[F[_]] {
  def currentCpuLoad: F[Double]
}

object CpuUtilization {

  def apply[F[_]](implicit M: CpuUtilization[F]): CpuUtilization[F] = M

  def reportProcessCpuLoad[F[_]: CpuUtilization: Monad: Capture: Metrics]: F[Unit] =
    for {
      load <- CpuUtilization[F].currentCpuLoad
      _    <- Metrics[F].setGauge("cpu-load", load.toLong)
    } yield ()
}
