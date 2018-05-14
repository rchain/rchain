package coop.rchain.node

import cats.Applicative

package object metrics {

  def jmxMetrics[F[_]: Applicative]: JmxMetrics[F] =
    new JmxMetrics[F] {

      import java.lang.management.ManagementFactory
      import javax.management.{Attribute, ObjectName}

      private val mBeanServer  = ManagementFactory.getOperatingSystemMXBean
      private val operatingSystem = mBeanServer.getObjectInstance()

//      private val operatingSystem = ManagementFactory.getOperatingSystemMXBean
//      val x = operatingSystem.getSystemLoadAverage

      def processCpu: F[ProcessCpu] = ???
      def memoryUsage: F[Memory] = ???
      def garbageCollectors: F[Seq[GarbageCollector]] = ???
      def threadPools: F[Seq[MemoryPool]] = ???
      def threads: F[Threads] = ???
    }

}
