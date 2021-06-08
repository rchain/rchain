package coop.rchain.node.runtime

import cats.effect.{Concurrent, ConcurrentEffect, ExitCode, Timer}
import cats.syntax.all._
import com.typesafe.config.Config
import coop.rchain.comm.discovery
import coop.rchain.comm.discovery.{KademliaHandleRPC, KademliaStore, NodeDiscovery}
import coop.rchain.comm.protocol.routing.Protocol
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.transport.{Blob, CommunicationResponse, GrpcTransportServer}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.monix.Monixable
import coop.rchain.node.api.{AdminWebApi, WebApi}
import coop.rchain.node.configuration.NodeConf
import coop.rchain.node.diagnostics.{
  BatchInfluxDBReporter,
  NewPrometheusReporter,
  UdpInfluxDBReporter
}
import coop.rchain.node.effects.EventConsumer
import coop.rchain.node.web.ReportingRoutes.ReportingHttpRoutes
import coop.rchain.node.{api, web}
import coop.rchain.shared.Log
import kamon.Kamon
import kamon.system.SystemMetrics
import kamon.zipkin.ZipkinReporter
import monix.execution.Scheduler

/**
  * Container for all servers Node provides
  */
final case class ServersInstances[F[_]](
    kademliaServer: fs2.Stream[F, Unit],
    transportServer: fs2.Stream[F, Unit],
    externalApiServer: fs2.Stream[F, Unit],
    internalApiServer: fs2.Stream[F, Unit],
    httpServer: fs2.Stream[F, ExitCode],
    adminHttpServer: fs2.Stream[F, ExitCode]
)

object ServersInstances {
  // format: off
  def build[F[_]
  /* Execution */   : Concurrent: Monixable: ConcurrentEffect
  /* P2P */         : NodeDiscovery: KademliaStore: ConnectionsCell: RPConfAsk
  /* Diagnostics */ : Metrics :Log:EventConsumer: Timer] // format: on
  (
      apiServers: APIServers,
      reportingRoutes: ReportingHttpRoutes[F],
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

    val transportServerStream = fs2
      .Stream(
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
      )
      .evalMap(
        _.start(
          grpcPacketHandler,
          grpcStreamHandler
        ) >> Log[F].info(s"Listening for traffic on $address.")
      )

    val kademliaServerStream = fs2.Stream
      .eval(
        discovery
          .acquireKademliaRPCServer(
            nodeConf.protocolServer.networkId,
            nodeConf.peersDiscovery.port,
            KademliaHandleRPC.handlePing[F],
            KademliaHandleRPC.handleLookup[F]
          )
      )
      .evalMap(
        server =>
          server.start >> Log[F]
            .info(s"Kademlia RPC server started at $host:${server.port}")
      )

    val externalApiServerStream = fs2.Stream
      .eval(
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
      )
      .evalMap(
        server =>
          server.start >> Log[F]
            .info(s"External API server started at ${nodeConf.apiServer.host}:${server.port}")
      )

    val internalApiServerStream = fs2.Stream
      .eval(
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
      )
      .evalMap(
        server =>
          server.start >> Log[F]
            .info(s"Internal API server started at $host:${server.port}")
      )

    val prometheusReporter = new NewPrometheusReporter()

    for {
      httpServerStream <- web.aquireHttpServer[F](
                           nodeConf.apiServer.enableReporting,
                           nodeConf.apiServer.host,
                           nodeConf.apiServer.portHttp,
                           prometheusReporter,
                           nodeConf.apiServer.maxConnectionIdle,
                           webApi,
                           reportingRoutes
                         )
      // Note - here http servers are not really stated, only worker streams are created.
      _ <- Log[F].info(
            s"HTTP API server started at ${nodeConf.apiServer.host}:${nodeConf.apiServer.portHttp}"
          )
      adminHttpServerStream <- web.aquireAdminHttpServer[F](
                                nodeConf.apiServer.host,
                                nodeConf.apiServer.portAdminHttp,
                                nodeConf.apiServer.maxConnectionIdle,
                                adminWebApi,
                                reportingRoutes
                              )

      _ <- Log[F].info(
            s"Admin HTTP API server started at ${nodeConf.apiServer.host}:${nodeConf.apiServer.portAdminHttp}"
          )

      _ = Kamon.reconfigure(kamonConf.withFallback(Kamon.config()))
      _ = if (nodeConf.metrics.influxdb) Kamon.addReporter(new BatchInfluxDBReporter())
      _ = if (nodeConf.metrics.influxdbUdp) Kamon.addReporter(new UdpInfluxDBReporter())
      _ = if (nodeConf.metrics.prometheus) Kamon.addReporter(prometheusReporter)
      _ = if (nodeConf.metrics.zipkin) Kamon.addReporter(new ZipkinReporter())
      _ = if (nodeConf.metrics.sigar) SystemMetrics.startCollecting()

    } yield ServersInstances(
      kademliaServerStream,
      transportServerStream,
      externalApiServerStream,
      internalApiServerStream,
      httpServerStream,
      adminHttpServerStream
    )
  }
}
