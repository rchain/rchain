package coop.rchain.node.diagnostics.client

import cats._
import cats.implicits._
import coop.rchain.node.effects.ConsoleIO
import coop.rchain.node.model.diagnostics._
import coop.rchain.shared.LongOps._

import scala.concurrent.duration._
import coop.rchain.comm.PeerNode

object Runtime {
  def diagnosticsProgram[F[_]: Monad: ConsoleIO: DiagnosticsService]: F[Unit] =
    for {
      peers   <- DiagnosticsService[F].listPeers
      _       <- ConsoleIO[F].println(showPeers(peers))
      core    <- DiagnosticsService[F].nodeCoreMetrics
      _       <- ConsoleIO[F].println(showNodeCoreMetrics(core))
      cpu     <- DiagnosticsService[F].processCpu
      _       <- ConsoleIO[F].println(showProcessCpu(cpu))
      mem     <- DiagnosticsService[F].memoryUsage
      _       <- ConsoleIO[F].println(showMemoryUsage(mem))
      pools   <- DiagnosticsService[F].memoryPools
      _       <- ConsoleIO[F].println(showMemoryPools(pools))
      gc      <- DiagnosticsService[F].garbageCollectors
      _       <- ConsoleIO[F].println(showGarbageCollectors(gc))
      threads <- DiagnosticsService[F].threads
      _       <- ConsoleIO[F].println(showThreads(threads))
      store   <- DiagnosticsService[F].store
      _       <- ConsoleIO[F].println(showStoreUsage(store))
      _       <- ConsoleIO[F].close
    } yield ()

  def showPeers(peers: Seq[PeerNode]): String =
    List(
      "List of peers:",
      peers.map(_.toAddress).mkString("\n")
    ).mkString("\n")

  def showProcessCpu(processCpu: ProcessCpu): String = {
    val time = processCpu.time.map(Duration.fromNanos(_).toMillis).getOrElse(0L)
    s"""Process CPU:
       |  - CPU load: ${processCpu.load.map(_ * 100).map("%.1f".format(_)).getOrElse("0.0")}%
       |  - CPU time: ${time}ms
       |""".stripMargin
  }

  def showMemory(memory: Memory): String =
    s"""    - committed: ${memory.committed} bytes
       |    - init: ${memory.init} bytes
       |    - max: ${memory.max.getOrElse(-1)} bytes
       |    - used: ${memory.used} bytes""".stripMargin

  def showMemoryUsage(memoryUsage: MemoryUsage): String =
    List(
      "Memory usage:",
      "  + Heap memory",
      memoryUsage.heap.map(showMemory).getOrElse(""),
      "  + Non-Heap memory",
      memoryUsage.nonHeap.map(showMemory).getOrElse("")
    ).mkString("", "\n", "\n")

  def showMemoryPool(memoryPool: MemoryPool): String =
    List(
      s"  + Name: ${memoryPool.name}",
      s"  + Type: ${memoryPool.poolType}",
      "  + Usage:",
      memoryPool.usage.map(showMemory).getOrElse(""),
      "  + Peak usage:",
      memoryPool.peakUsage.map(showMemory).getOrElse("")
    ).mkString("", "\n", "\n")

  def showMemoryPools(pools: Seq[MemoryPool]): String =
    List(
      "Memory pools:",
      pools.map(showMemoryPool).mkString("\n")
    ).mkString("\n")

  def showGarbageCollector(garbageCollector: GarbageCollector): String = {
    val startTime = garbageCollector.startTime.getOrElse(-1L)
    val endTime   = garbageCollector.endTime.getOrElse(-1L)
    val duration  = garbageCollector.duration.getOrElse(-1L)
    s"""  + Name: ${garbageCollector.name}
       |    - Total collections: ${garbageCollector.totalCollections}
       |    - Total collection time: ${garbageCollector.totalCollectionTime}ms
       |    - Current GC start time: ${startTime}ms
       |    - Current GC end time: ${endTime}ms
       |    - Current GC duration: ${duration}ms
       |""".stripMargin
  }

  def showGarbageCollectors(garbageCollectors: Seq[GarbageCollector]): String =
    List(
      "Garbage collectors:",
      garbageCollectors.map(showGarbageCollector).mkString("\n")
    ).mkString("\n")

  def showThreads(threads: Threads): String =
    s"""Threads:
       |  - Threads: ${threads.threadCount}
       |  - Daemon threads: ${threads.daemonThreadCount}
       |  - Peak threads: ${threads.peakThreadCount}
       |  - Total started threads: ${threads.totalStartedThreadCount}
       |""".stripMargin

  def showNodeCoreMetrics(nodeCoreMetrics: NodeCoreMetrics): String =
    s"""Node core metrics:
       |  - Ping receivers: ${nodeCoreMetrics.pingReceiverCount}
       |  - Lookup receivers: ${nodeCoreMetrics.lookupReceiverCount}
       |  - Disconnect receivers: ${nodeCoreMetrics.disconnectReceiverCount}
       |  - Connects: ${nodeCoreMetrics.connects}
       |  - P2P encryption handshake receivers: ${nodeCoreMetrics.p2PEncryptionHandshakeReceiverCount}
       |  - P2P protocol handshake receivers: ${nodeCoreMetrics.p2PProtocolHandshakeReceiverCount}
       |  - Peers: ${nodeCoreMetrics.peers}
       |""".stripMargin

  def showStoreUsage(storeUsage: StoreUsage): String = {
    def writeCounts(rspaceName: String)(name: String, value: Option[RSpaceUsageMetric]): String =
      s"""
         |  + $rspaceName $name
         |    - Total Count: ${value.map(_.count).getOrElse(0)}
         |    - Average (ms): ${value.map(_.avgMilliseconds.formatted("%.2f")).getOrElse("-")}
         |    - Peak Rate (events/sec): ${value.map(_.peakRate).getOrElse(0)}
         |    - Current Rate (events/sec): ${value.map(_.currentRate).getOrElse(0)}
          """

    def writeCommCounts(rspaceName: String)(name: String,
                                            value: Option[RSpaceUsageMetric]): String =
      s"""
         |  + $rspaceName $name
         |    - Total Count: ${value.map(_.count).getOrElse(0)}
         |    - Peak Rate (events/sec): ${value.map(_.peakRate).getOrElse(0)}
         |    - Current Rate (events/sec): ${value.map(_.currentRate).getOrElse(0)}
          """
    def writeRSpaceMetrics(rspaceName: String)(maybeRSpaceUsage: Option[RSpaceUsage]) =
      s"""
       |  ${writeCounts(rspaceName)("Consumes", maybeRSpaceUsage.flatMap(_.consumes))}
       |  ${writeCounts(rspaceName)("Produces", maybeRSpaceUsage.flatMap(_.produces))}
       |  ${writeCommCounts(rspaceName)("Consumes COMM", maybeRSpaceUsage.flatMap(_.consumesComm))}
       |  ${writeCommCounts(rspaceName)("Produces COMM", maybeRSpaceUsage.flatMap(_.producesComm))}
       |  ${writeCommCounts(rspaceName)("Install COMM", maybeRSpaceUsage.flatMap(_.installComm))}
      """.stripMargin

    s"""Store metrics:
       |  - Total Size On Disk: ${storeUsage.totalSizeOnDisk.toHumanReadableSize}
       |  - RSpace & ReplayRSpace Size On Disk: ${storeUsage.rspaceSizeOnDisk.toHumanReadableSize}
       |  - RSpace & ReplayRSpace Data Entries: ${storeUsage.rspaceDataEntries}""".stripMargin + writeRSpaceMetrics(
      "RSpace")(storeUsage.rspace) + writeRSpaceMetrics("ReplayRSpace")(storeUsage.replayRSpace)
  }
}
