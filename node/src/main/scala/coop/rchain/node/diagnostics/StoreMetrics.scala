package coop.rchain.node.diagnostics

import cats.Monad
import cats.data.EitherT
import cats.implicits._
import coop.rchain.catscontrib.MonadTrans
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.metrics.Metrics
import coop.rchain.node.model.diagnostics.{StoreUsage, StoreUsageCount}

trait StoreMetrics[F[_]] {
  def storeUsage: F[StoreUsage]
}

object StoreMetrics extends StoreMetricsInstances {
  def apply[F[_]](implicit M: StoreMetrics[F]): StoreMetrics[F] = M

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: StoreMetrics[F]): StoreMetrics[T[F, ?]] =
    new StoreMetrics[T[F, ?]] {
      def storeUsage: T[F, StoreUsage] = C.storeUsage.liftM[T]
    }

  def report[F[_]: Monad: StoreMetrics: Metrics]: F[Unit] = {
    val m      = Metrics[F]
    val storem = StoreMetrics[F]

    def g(name: String, value: Long): F[Unit] =
      m.setGauge(name, value)

    def cm(name: String, value: Option[StoreUsageCount]): F[Unit] =
      for {
        _ <- m.setGauge(name + "-count", value.map(_.count).getOrElse(0))
        _ <- m.setGauge(name + "-peak-rate", value.map(_.peakRate.toLong).getOrElse(0))
        _ <- m.setGauge(name + "-current-rate", value.map(_.currentRate.toLong).getOrElse(0))
      } yield ()

    def pc(name: String, value: Option[StoreUsageCount]): F[Unit] =
      for {
        _ <- cm(name, value)
        _ <- m.setGauge(name + "-avg-ms", value.map(_.avgMilliseconds.toLong).getOrElse(0))
      } yield ()

    def reportStoreSize(storeSize: StoreUsage): List[F[Unit]] =
      List(
        g("total-size-on-disk", storeSize.totalSizeOnDisk),
        g("rspace-size-on-disk", storeSize.rspaceSizeOnDisk),
        g("rspace-data-entries", storeSize.rspaceDataEntries),
        pc("rspace-consumes", storeSize.rspaceConsumesCount),
        pc("rspace-produces", storeSize.rspaceProducesCount),
        cm("rspace-consumes-COMM", storeSize.rspaceConsumesCommCount),
        cm("rspace-produces-COMM", storeSize.rspaceProducesCommCount),
        cm("rspace-install-COMM", storeSize.rspaceInstallCommCount)
      )

    def join(tasks: Seq[F[Unit]]*): F[List[Unit]] =
      tasks.toList.flatten.sequence

    for {
      storeSize <- storem.storeUsage
      _         <- join(reportStoreSize(storeSize))
    } yield ()
  }
}

sealed abstract class StoreMetricsInstances {
  implicit def eitherTStoreMetrics[E, F[_]: Monad: StoreMetrics[?[_]]]
    : StoreMetrics[EitherT[F, E, ?]] =
    StoreMetrics.forTrans[F, EitherT[?[_], E, ?]]
}
