package coop.rchain.node.runtime

import cats.effect.{Concurrent, ConcurrentEffect, ExitCode, Timer}
import cats.syntax.all._
import com.typesafe.config.Config
import coop.rchain.casper.ReportingCasper
import coop.rchain.comm.discovery
import coop.rchain.comm.discovery.{KademliaHandleRPC, KademliaStore, NodeDiscovery}
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.{Blob, CommunicationResponse, GrpcTransportServer}
import coop.rchain.metrics.Metrics
import coop.rchain.monix.Monixable
import coop.rchain.node.api.{AdminWebApi, WebApi}
import coop.rchain.node.configuration.NodeConf
import coop.rchain.node.diagnostics.{
  BatchInfluxDBReporter,
  NewPrometheusReporter,
  UdpInfluxDBReporter
}
import coop.rchain.node.effects.EventConsumer
import coop.rchain.node.{api, web}
import coop.rchain.shared.Log
import fs2.Stream
import fs2.concurrent.SignallingRef
import kamon.Kamon
import kamon.system.SystemMetrics
import kamon.zipkin.ZipkinReporter
import monix.execution.Scheduler

/**
  * Container for all servers Node provides
  */
final case class ServersInstances[F[_]](
    kademliaServer: fs2.Stream[F, ExitCode],
    transportServer: fs2.Stream[F, ExitCode],
    externalApiServer: fs2.Stream[F, ExitCode],
    internalApiServer: fs2.Stream[F, ExitCode],
    httpServer: fs2.Stream[F, ExitCode],
    adminHttpServer: fs2.Stream[F, ExitCode],
    kamonReporter: fs2.Stream[F, ExitCode]
)

object ServersInstances {
  // format: off
  def build[F[_]
  /* Execution */ : Concurrent : Monixable : ConcurrentEffect
  /* P2P */ : NodeDiscovery : KademliaStore : ConnectionsCell : RPConfAsk
  /* Diagnostics */ : Metrics : Log : EventConsumer : Timer] // format: on
  (
      apiServers: APIServers,
      reportingCasper: ReportingCasper[F],
      webApi: WebApi[F],
      adminWebApi: AdminWebApi[F],
      grpcPacketHandler: Protocol => F[CommunicationResponse],
      grpcStreamHandler: Blob => F[Unit],
      host: String,
      address: String,
      nodeConf: NodeConf,
      kamonConf: Config,
      grpcScheduler: Scheduler
  )(implicit scheduler: Scheduler): F[ServersInstances[F]] = {

    val kademliaStartupMsg = s"Kademlia RPC server started at $host:${nodeConf.peersDiscovery.port}"
    val extenalRPCStartupMsg =
      s"External RPC server started at ${nodeConf.apiServer.host}:${nodeConf.apiServer.portGrpcExternal}"
    val internalRPCStartupMsg =
      s"Internal RPC server started at $host:${nodeConf.apiServer.portGrpcInternal}"
    val extenalHTTPStartupMsg =
      s"External HTTP API server started at ${nodeConf.apiServer.host}:${nodeConf.apiServer.portHttp}"
    val internalHTTPStartupMsg =
      s"Internal HTTP API server started at ${nodeConf.apiServer.host}:${nodeConf.apiServer.portAdminHttp}"
    val kamonReporterStartupMsg    = s"Starting Kamon reporters."
    val transportServerStartupMsg  = s"Listening for traffic on $address."
    val kademliaShutdownMsg        = s"Shutting down Kademlia RPC server."
    val extenalRPCShutdownMsg      = s"Shutting down external RPC server."
    val internalRPCShutdownMsg     = s"Shutting down internal RPC server."
    val extenalHTTPShutdownMsg     = s"Shutting down external HTTP API server."
    val internalHTTPShutdownMsg    = s"Shutting down internal HTTP API server."
    val kamonReporterShutdownMsg   = s"Shutting down Kamon reporters."
    val transportServerShutdownMsg = s"Shutting down transport server."

    val prometheusReporter = new NewPrometheusReporter()

    val transportServer =
      GrpcTransportServer
        .acquireServer[F](
          nodeConf.protocolServer.networkId,
          nodeConf.protocolServer.port,
          nodeConf.tls.certificatePath,
          nodeConf.tls.keyPath,
          nodeConf.protocolServer.grpcMaxRecvMessageSize.toInt,
          nodeConf.protocolServer.grpcMaxRecvStreamMessageSize,
          nodeConf.protocolServer.maxMessageConsumers
        )

    val kademliaServer =
      discovery
        .acquireKademliaRPCServer(
          nodeConf.protocolServer.networkId,
          nodeConf.peersDiscovery.port,
          KademliaHandleRPC.handlePing[F],
          KademliaHandleRPC.handleLookup[F]
        )

    val externalApiServer =
      api.acquireExternalServer[F](
        nodeConf.apiServer.host,
        nodeConf.apiServer.portGrpcExternal,
        grpcScheduler,
        apiServers.deploy,
        nodeConf.apiServer.grpcMaxRecvMessageSize.toInt,
        nodeConf.apiServer.keepAliveTime,
        nodeConf.apiServer.keepAliveTimeout,
        nodeConf.apiServer.permitKeepAliveTime,
        nodeConf.apiServer.maxConnectionIdle,
        nodeConf.apiServer.maxConnectionAge,
        nodeConf.apiServer.maxConnectionAgeGrace
      )

    val internalApiServer =
      api.acquireInternalServer[F](
        nodeConf.apiServer.host,
        nodeConf.apiServer.portGrpcInternal,
        grpcScheduler,
        apiServers.repl,
        apiServers.deploy,
        apiServers.propose,
        nodeConf.apiServer.grpcMaxRecvMessageSize.toInt,
        nodeConf.apiServer.keepAliveTime,
        nodeConf.apiServer.keepAliveTimeout,
        nodeConf.apiServer.permitKeepAliveTime,
        nodeConf.apiServer.maxConnectionIdle,
        nodeConf.apiServer.maxConnectionAge,
        nodeConf.apiServer.maxConnectionAgeGrace
      )

    @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
    val reconfigureKamon =
      Concurrent[F].delay({
        Kamon.reconfigure(kamonConf.withFallback(Kamon.config()))
        if (nodeConf.metrics.influxdb) Kamon.addReporter(new BatchInfluxDBReporter())
        if (nodeConf.metrics.influxdbUdp) Kamon.addReporter(new UdpInfluxDBReporter())
        if (nodeConf.metrics.prometheus) Kamon.addReporter(prometheusReporter)
        if (nodeConf.metrics.zipkin) Kamon.addReporter(new ZipkinReporter())
        if (nodeConf.metrics.sigar) SystemMetrics.startCollecting()
      })

    for {
      httpServerStream <- web.aquireHttpServer[F](
                           nodeConf.apiServer.enableReporting,
                           nodeConf.apiServer.host,
                           nodeConf.apiServer.portHttp,
                           prometheusReporter,
                           reportingCasper,
                           webApi,
                           nodeConf.apiServer.maxConnectionIdle
                         )
      adminHttpServerStream <- web.aquireAdminHttpServer[F](
                                nodeConf.apiServer.host,
                                nodeConf.apiServer.portAdminHttp,
                                adminWebApi,
                                nodeConf.apiServer.maxConnectionIdle
                              )

      // single value discrete stream that won't ever emit true
      discreteFalseStream <- SignallingRef[F, Boolean](false).map(_.discrete)
      runForever = (stream: Stream[F, Unit]) =>
        stream *> (discreteFalseStream.takeWhile(_ === false).drain ++ Stream(ExitCode.Success))

      kamonReporterStream = runForever(
        Stream.bracket(Log[F].info(kamonReporterStartupMsg) >> reconfigureKamon)(
          _ =>
            Concurrent[F].delay(Kamon.stopAllReporters()) *> Log[F].info(kamonReporterShutdownMsg)
        )
      )

      transportServerStream = runForever(
        Stream.bracket(
          transportServer.start(grpcPacketHandler, grpcStreamHandler) >> Log[F]
            .info(transportServerStartupMsg)
        )(_ => transportServer.stop() *> Log[F].info(transportServerShutdownMsg))
      )

      kademliaServerStream = runForever(
        Stream.bracket(
          kademliaServer.start >> Log[F].info(kademliaStartupMsg)
        )(_ => kademliaServer.stop *> Log[F].info(kademliaShutdownMsg))
      )

      externalApiServerStream = runForever(
        Stream.bracket(
          externalApiServer.start >> Log[F].info(extenalRPCStartupMsg)
        )(_ => externalApiServer.stop *> Log[F].info(extenalRPCShutdownMsg))
      )

      internalApiServerStream = runForever(
        Stream.bracket(
          internalApiServer.start >> Log[F].info(internalRPCStartupMsg)
        )(_ => internalApiServer.stop *> Log[F].info(internalRPCShutdownMsg))
      )

    } yield ServersInstances(
      kademliaServerStream,
      transportServerStream,
      externalApiServerStream,
      internalApiServerStream,
      httpServerStream
        .evalTap(_ => Log[F].info(extenalHTTPStartupMsg))
        .onFinalize(Log[F].info(extenalHTTPShutdownMsg)),
      adminHttpServerStream
        .evalTap(_ => Log[F].info(internalHTTPStartupMsg))
        .onFinalize(Log[F].info(internalHTTPShutdownMsg)),
      kamonReporterStream
    )
  }
}
