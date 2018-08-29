package coop.rchain.node.diagnostics.client

import cats._
import cats.implicits._
import coop.rchain.node.effects.ConsoleIO
import coop.rchain.node.model.diagnostics._
import coop.rchain.catscontrib._
import coop.rchain.catscontrib.Catscontrib._

import scala.concurrent.duration._
import coop.rchain.comm.PeerNode

object Runtime {

  type ErrorHandler[F[_]] = ApplicativeError_[F, Throwable]

  def diagnosticsProgram[F[_]: Monad: ErrorHandler: ConsoleIO: DiagnosticsService]: F[Unit] = {
    val program = for {
      peersRP <- DiagnosticsService[F].listPeers
      _       <- ConsoleIO[F].println(showPeers(peersRP, "connected"))
      peersND <- DiagnosticsService[F].listDiscoveredPeers
      _       <- ConsoleIO[F].println(showPeers(peersND, "discovered"))
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
      _       <- ConsoleIO[F].close
    } yield ()

    for {
      result <- program.attempt
      _ <- result match {
            case Left(ex) => ConsoleIO[F].println(s"Error: ${processError(ex).getMessage}")
            case _        => ().pure[F]
          }
    } yield ()
  }

  private def processError(t: Throwable): Throwable =
    Option(t.getCause).getOrElse(t)

  def showPeers(peers: Seq[PeerNode], peerType: String): String =
    List(
      s"List of $peerType peers:",
      peers.map(_.toAddress).mkString("\n")
    ).mkString("", "\n", "\n")

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
       |  - From: ${nodeCoreMetrics.from}
       |  - To: ${nodeCoreMetrics.to}
       |""".stripMargin

}
