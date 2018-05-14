package coop.rchain.node.metrics

import cats.Monad
import cats.data.EitherT

import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.catscontrib.MonadTrans

case class ProcessCpu(
  // amount of CPU load, as a value between 0.0 and 1.0
  load: Double,
  // CPU time used by the process on which the JVM is running in nanoseconds
  time: Long
)

case class Memory(
  // amount of memory committed in bytes
  committed: Long,
  // amount of memory that the JVM initially requested in bytes
  init: Long,
  // maximum amount of memory possible in bytes
  max: Long,
  // amount of used memory in bytes
  used: Long
)

case class GarbageCollector(
  // name representing this memory manager
  name: String,
  // total number of collections that have occurred
  totalCollections: Long,
  // accumulated collection time in ms
  totalCollectionTime: Long,
  // start time of last GC since the JVM was started in ms
  startTime: Long,
  // end time of last GC since the JVM was started
  endTime: Long,
  // number of last GC threads
  threadCount: Int,
  // elapsed time of last GC in ms
  duration: Long
)

case class MemoryPool(
  // name representing this memory pool
  name: String,
  // type of this memory pool
  poolType: String,
  // memory pool usage
  usage: Memory,
  // peak memory usage
  peakUsage: Memory
)

case class Threads(
  // current number of live threads including both daemon and non-daemon threads
  threadCount: Int,
  // current number of live daemon threads.
  daemonThreadCount: Int,
  // peak live thread count since the JVM started
  peakThreadCount: Int,
  // total number of threads created and also started since the JVM started
  totalStartedThreadCount: Int
)

trait JmxMetrics[F[_]] {
  def processCpu: F[ProcessCpu]
  def memoryUsage: F[Memory]
  def garbageCollectors: F[Seq[GarbageCollector]]
  def threadPools: F[Seq[MemoryPool]]
  def threads: F[Threads]
}

object JmxMetrics extends JmxMetricsInstances {
  def apply[F[_]](implicit M: JmxMetrics[F]): JmxMetrics[F] = M

  def forTrans[F[_]: Monad, T[_[_], _]: MonadTrans](
    implicit C: JmxMetrics[F]): JmxMetrics[T[F, ?]] =
    new JmxMetrics[T[F, ?]] {
      def processCpu: T[F, ProcessCpu] = C.processCpu.liftM[T]
      def memoryUsage: T[F, Memory] = C.memoryUsage.liftM[T]
      def garbageCollectors: T[F, Seq[GarbageCollector]] = C.garbageCollectors.liftM[T]
      def threadPools: T[F, Seq[MemoryPool]] = C.threadPools.liftM[T]
      def threads: T[F, Threads] = C.threads.liftM[T]
    }
}

sealed abstract class JmxMetricsInstances {
  implicit def eitherTJmxMetrics[E, F[_]: Monad: JmxMetrics[?[_]]]
  : JmxMetrics[EitherT[F, E, ?]] =
    JmxMetrics.forTrans[F, EitherT[?[_], E, ?]]
}
