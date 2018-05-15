package coop.rchain.node

import java.lang.management.ManagementFactory

import scala.collection.JavaConverters._

import cats.Applicative
import cats.implicits._

package object metrics {

  def jmxMetrics[F[_]: Applicative]: JmxMetrics[F] =
    new JmxMetrics[F] {

      private def check[A](a: A)(implicit n: Numeric[A]): Option[A] = checkOpt(Some(a))

      private def checkOpt[A](a: Option[A])(implicit n: Numeric[A]): Option[A] =
        a.filter(n.gteq(_, n.zero))

      def processCpu: F[ProcessCpu] =
        ManagementFactory.getOperatingSystemMXBean match {
          case b: com.sun.management.OperatingSystemMXBean =>
            ProcessCpu(check(b.getProcessCpuLoad), check(b.getProcessCpuTime)).pure[F]
          case _ => ProcessCpu().pure[F]
        }

      def memoryUsage: F[MemoryUsage] = {
        val b       = ManagementFactory.getMemoryMXBean
        val heap    = b.getHeapMemoryUsage
        val nonHeap = b.getNonHeapMemoryUsage
        MemoryUsage(
          Memory(
            committed = heap.getCommitted,
            init = heap.getInit,
            max = heap.getMax,
            used = heap.getUsed
          ),
          Memory(
            committed = nonHeap.getCommitted,
            init = nonHeap.getInit,
            max = nonHeap.getMax,
            used = nonHeap.getUsed
          )
        ).pure[F]
      }

      def garbageCollectors: F[Seq[GarbageCollector]] =
        ManagementFactory.getGarbageCollectorMXBeans.asScala
          .map {
            case b: com.sun.management.GarbageCollectorMXBean =>
              val last = Option(b.getLastGcInfo)
              GarbageCollector(
                name = b.getName,
                totalCollections = b.getCollectionCount,
                totalCollectionTime = b.getCollectionTime,
                startTime = checkOpt(last.map(_.getStartTime)),
                endTime = checkOpt(last.map(_.getEndTime)),
                duration = checkOpt(last.map(_.getDuration))
              )
            case b =>
              GarbageCollector(
                name = b.getName,
                totalCollections = b.getCollectionCount,
                totalCollectionTime = b.getCollectionTime
              )
          }
          .toSeq
          .pure[F]

      def memoryPools: F[Seq[MemoryPool]] =
        ManagementFactory.getMemoryPoolMXBeans.asScala
          .map { b =>
            val usage     = b.getUsage
            val peakUsage = b.getPeakUsage
            MemoryPool(
              name = b.getName,
              poolType = b.getType.toString,
              usage = Memory(
                committed = usage.getCommitted,
                init = usage.getInit,
                max = usage.getMax,
                used = usage.getUsed
              ),
              peakUsage = Memory(
                committed = peakUsage.getCommitted,
                init = peakUsage.getInit,
                max = peakUsage.getMax,
                used = peakUsage.getUsed
              )
            )
          }
          .toSeq
          .pure[F]

      def threads: F[Threads] = {
        val b = ManagementFactory.getThreadMXBean
        Threads(
          threadCount = b.getThreadCount,
          daemonThreadCount = b.getDaemonThreadCount,
          peakThreadCount = b.getPeakThreadCount,
          totalStartedThreadCount = b.getTotalStartedThreadCount
        ).pure[F]
      }
    }

}
