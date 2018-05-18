package coop.rchain.node.metrics

import cats.Monad
import cats.data.EitherT

import coop.rchain.catscontrib.MonadTrans
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.node.model.metrics._

trait JvmMetrics[F[_]] {
  def processCpu: F[ProcessCpu]
  def memoryUsage: F[MemoryUsage]
  def garbageCollectors: F[Seq[GarbageCollector]]
  def memoryPools: F[Seq[MemoryPool]]
  def threads: F[Threads]
}

object JvmMetrics extends JmxMetricsInstances {
  def apply[F[_]](implicit M: JvmMetrics[F]): JvmMetrics[F] = M

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
      implicit C: JvmMetrics[F]): JvmMetrics[T[F, ?]] =
    new JvmMetrics[T[F, ?]] {
      def processCpu: T[F, ProcessCpu]                   = C.processCpu.liftM[T]
      def memoryUsage: T[F, MemoryUsage]                 = C.memoryUsage.liftM[T]
      def garbageCollectors: T[F, Seq[GarbageCollector]] = C.garbageCollectors.liftM[T]
      def memoryPools: T[F, Seq[MemoryPool]]             = C.memoryPools.liftM[T]
      def threads: T[F, Threads]                         = C.threads.liftM[T]
    }
}

sealed abstract class JmxMetricsInstances {
  implicit def eitherTJmxMetrics[E, F[_]: Monad: JvmMetrics[?[_]]]: JvmMetrics[EitherT[F, E, ?]] =
    JvmMetrics.forTrans[F, EitherT[?[_], E, ?]]
}
