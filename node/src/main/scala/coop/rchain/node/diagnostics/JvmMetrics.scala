package coop.rchain.node.diagnostics

import cats._, cats.data._, cats.implicits._

import coop.rchain.catscontrib.MonadTrans
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.metrics.Metrics
import coop.rchain.node.model.diagnostics._

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
      implicit C: JvmMetrics[F]
  ): JvmMetrics[T[F, ?]] =
    new JvmMetrics[T[F, ?]] {
      def processCpu: T[F, ProcessCpu]                   = C.processCpu.liftM[T]
      def memoryUsage: T[F, MemoryUsage]                 = C.memoryUsage.liftM[T]
      def garbageCollectors: T[F, Seq[GarbageCollector]] = C.garbageCollectors.liftM[T]
      def memoryPools: T[F, Seq[MemoryPool]]             = C.memoryPools.liftM[T]
      def threads: T[F, Threads]                         = C.threads.liftM[T]
    }

  def report[F[_]: Monad: JvmMetrics: Metrics]: F[Unit] = {

    val m   = Metrics[F]
    val jvm = JvmMetrics[F]

    def g(name: String, value: Long): F[Unit] =
      m.setGauge(name, value)

    def reportProcessCpu(processCpu: ProcessCpu): List[F[Unit]] =
      List(
        g("process-cpu-time", processCpu.time.getOrElse(0)),
        g("process-cpu-load", processCpu.load.map(_ * 1000).map(_.toLong).getOrElse(0))
      )

    def reportMemory(memory: Memory, prefix: String): List[F[Unit]] =
      List(
        g(s"$prefix-commited", memory.committed),
        g(s"$prefix-init", memory.init),
        g(s"$prefix-max", memory.max.getOrElse(-1L)),
        g(s"$prefix-used", memory.used)
      )

    def reportMemoryUsage(memoryUsage: MemoryUsage): List[F[Unit]] =
      (memoryUsage.heap.map(reportMemory(_, "memory-heap")).toList ++
        memoryUsage.nonHeap.map(reportMemory(_, "memory-non-heap")).toList).flatten

    def reportGarbageCollector(garbageCollector: GarbageCollector): List[F[Unit]] = {
      val name = "gc-" + garbageCollector.name.replace(' ', '-').toLowerCase
      List(
        g(s"$name-total-collections", garbageCollector.totalCollections),
        g(s"$name-total-collection-time", garbageCollector.totalCollectionTime)
      ) ++ List(
        garbageCollector.startTime.map(g(s"$name-start-time", _)).toList,
        garbageCollector.endTime.map(g(s"$name-end-time", _)).toList,
        garbageCollector.duration.map(g(s"$name-duration", _)).toList
      ).flatten
    }

    def reportMemoryPool(memoryPool: MemoryPool): List[F[Unit]] = {
      val name = "mempool-" + memoryPool.name.replace(' ', '-').toLowerCase
      (memoryPool.usage.map(reportMemory(_, name)).toList ++
        memoryPool.peakUsage.map(reportMemory(_, s"$name-peak")).toList).flatten
    }

    def reportThreads(threads: Threads): List[F[Unit]] =
      List(
        g("thread-count", threads.threadCount.toLong),
        g("thread-count-daemon", threads.daemonThreadCount.toLong),
        g("thread-count-peak", threads.peakThreadCount.toLong),
        g("thread-total-started", threads.totalStartedThreadCount)
      )

    def join(tasks: Seq[F[Unit]]*): F[List[Unit]] =
      tasks.toList.flatten.sequence

    for {
      cpu     <- jvm.processCpu
      mem     <- jvm.memoryUsage
      gcs     <- jvm.garbageCollectors
      pools   <- jvm.memoryPools
      threads <- jvm.threads
      _ <- join(
            reportProcessCpu(cpu),
            reportMemoryUsage(mem),
            gcs.flatMap(reportGarbageCollector),
            pools.flatMap(reportMemoryPool),
            reportThreads(threads)
          )
    } yield ()

  }
}

sealed abstract class JmxMetricsInstances {
  implicit def eitherTJmxMetrics[E, F[_]: Monad: JvmMetrics[?[_]]]: JvmMetrics[EitherT[F, E, ?]] =
    JvmMetrics.forTrans[F, EitherT[?[_], E, ?]]
}
