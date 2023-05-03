package coop.rchain.node.runtime

import cats.effect.{Concurrent, ConcurrentEffect, Resource, Sync, Timer}
import cats.syntax.all._
import com.typesafe.config.Config
import coop.rchain.casper.protocol.deploy.v1.DeployServiceV1GrpcMonix
import coop.rchain.casper.protocol.propose.v1.ProposeServiceV1GrpcMonix
import coop.rchain.comm.discovery.{KademliaHandleRPC, KademliaStore, NodeDiscovery}
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.rp.HandleMessages
import coop.rchain.comm.transport.{GrpcTransportServer, TransportLayer}
import coop.rchain.comm.{discovery, RoutingMessage}
import coop.rchain.metrics.Metrics
import coop.rchain.monix.Monixable
import coop.rchain.node.api.{AdminWebApi, WebApi}
import coop.rchain.node.configuration.NodeConf
import coop.rchain.node.diagnostics.{
  BatchInfluxDBReporter,
  NewPrometheusReporter,
  UdpInfluxDBReporter
}
import coop.rchain.node.model.repl.ReplGrpcMonix
import coop.rchain.node.web.ReportingRoutes.ReportingHttpRoutes
import coop.rchain.node.{api, web}
import coop.rchain.sdk.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.concurrent.Queue
import io.grpc.Server
import kamon.Kamon
import kamon.system.SystemMetrics
import kamon.zipkin.ZipkinReporter
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.server

object NetworkServers {

  /**
    * Creates API network service (gRPC and HTTP). Exposed as a [[Resource]] to gracefully shutdown.
    */
  // format: off
  def create[F[_]
    /* Execution */   : Monixable: ConcurrentEffect: Timer
    /* Comm */        : TransportLayer: NodeDiscovery: KademliaStore: RPConfAsk: ConnectionsCell
    /* Diagnostics */ : Log: Metrics] // format: on
  (
      routingMessageQueue: Queue[F, RoutingMessage],
      grpcServices: GrpcServices,
      webApi: WebApi[F],
      adminWebApi: AdminWebApi[F],
      reportingRoutes: ReportingHttpRoutes[F],
      nodeConf: NodeConf,
      kamonConf: Config,
      grpcScheduler: Scheduler
  )(implicit scheduler: Scheduler): Resource[F, Unit] = {
    val GrpcServices(deploySrv, proposeSrv, replSrv) = grpcServices
    val host                                         = nodeConf.apiServer.host
    for {
      nodeAddress <- Resource.eval(RPConfAsk[F].ask.map(_.local.toAddress))

      intServer <- internalServer(nodeConf, replSrv, deploySrv, proposeSrv, grpcScheduler)
      _         <- Resource.eval(Log[F].info(s"Internal API server started at $host:${intServer.getPort}."))

      extServer    <- externalServer(nodeConf, deploySrv, grpcScheduler)
      extServerMsg = s"External API server started at $host:${extServer.getPort}."
      _            <- Resource.eval(Log[F].info(extServerMsg))

      _ <- protocolServer(nodeConf, routingMessageQueue)
      _ <- Resource.eval(Log[F].info(s"Listening for traffic on $nodeAddress."))

      discovery <- discoveryServer(nodeConf, grpcScheduler)
      _         <- Resource.eval(Log[F].info(s"Kademlia RPC server started at $host:${discovery.getPort}."))

      prometheusRep = new NewPrometheusReporter()

      _         <- webApiServer(nodeConf, webApi, reportingRoutes, prometheusRep)
      webApiMsg = s"HTTP API server started at $host:${nodeConf.apiServer.portHttp}."
      _         <- Resource.eval(Log[F].info(webApiMsg))

      _              <- adminWebApiServer(nodeConf, webApi, adminWebApi, reportingRoutes)
      adminWebApiMsg = s"Admin HTTP API server started at $host:${nodeConf.apiServer.portAdminHttp}."
      _              <- Resource.eval(Log[F].info(adminWebApiMsg))

      _ <- metricsInit(nodeConf, kamonConf, prometheusRep)
    } yield ()
  }

  def internalServer[F[_]: Concurrent: Log](
      nodeConf: NodeConf,
      replService: ReplGrpcMonix.Repl,
      deployService: DeployServiceV1GrpcMonix.DeployService,
      proposeService: ProposeServiceV1GrpcMonix.ProposeService,
      grpcScheduler: Scheduler
  ): Resource[F, Server] =
    api.acquireInternalServer[F](
      nodeConf.apiServer.host,
      nodeConf.apiServer.portGrpcInternal,
      grpcScheduler,
      replService,
      deployService,
      proposeService,
      nodeConf.apiServer.grpcMaxRecvMessageSize.toInt,
      nodeConf.apiServer.keepAliveTime,
      nodeConf.apiServer.keepAliveTimeout,
      nodeConf.apiServer.permitKeepAliveTime,
      nodeConf.apiServer.maxConnectionIdle,
      nodeConf.apiServer.maxConnectionAge,
      nodeConf.apiServer.maxConnectionAgeGrace
    )

  def externalServer[F[_]: Concurrent: Log](
      nodeConf: NodeConf,
      deployService: DeployServiceV1GrpcMonix.DeployService,
      grpcScheduler: Scheduler
  ): Resource[F, Server] =
    api.acquireExternalServer[F](
      nodeConf.apiServer.host,
      nodeConf.apiServer.portGrpcExternal,
      grpcScheduler,
      deployService,
      nodeConf.apiServer.grpcMaxRecvMessageSize.toInt,
      nodeConf.apiServer.keepAliveTime,
      nodeConf.apiServer.keepAliveTimeout,
      nodeConf.apiServer.permitKeepAliveTime,
      nodeConf.apiServer.maxConnectionIdle,
      nodeConf.apiServer.maxConnectionAge,
      nodeConf.apiServer.maxConnectionAgeGrace
    )

  def protocolServer[F[_]: Monixable: Concurrent: TransportLayer: ConnectionsCell: RPConfAsk: Log: Metrics: Timer](
      nodeConf: NodeConf,
      routingMessageQueue: Queue[F, RoutingMessage]
  )(implicit scheduler: Scheduler): Resource[F, Unit] = {
    val server = GrpcTransportServer.acquireServer[F](
      nodeConf.protocolServer.networkId,
      nodeConf.protocolServer.port,
      nodeConf.tls.certificatePath,
      nodeConf.tls.keyPath,
      nodeConf.protocolServer.grpcMaxRecvMessageSize.toInt,
      nodeConf.protocolServer.grpcMaxRecvStreamMessageSize,
      nodeConf.protocolServer.maxMessageConsumers
    )

    server.resource(
      HandleMessages.handle[F](_, routingMessageQueue),
      blob => routingMessageQueue.enqueue1(RoutingMessage(blob.sender, blob.packet))
    )
  }

  def discoveryServer[F[_]: Monixable: Concurrent: KademliaStore: Log: Metrics](
      nodeConf: NodeConf,
      grpcScheduler: Scheduler
  ): Resource[F, Server] =
    discovery.acquireKademliaRPCServer(
      nodeConf.protocolServer.networkId,
      nodeConf.peersDiscovery.port,
      KademliaHandleRPC.handlePing[F],
      KademliaHandleRPC.handleLookup[F],
      grpcScheduler
    )

  def webApiServer[F[_]: ConcurrentEffect: Timer: NodeDiscovery: ConnectionsCell: RPConfAsk: Log](
      nodeConf: NodeConf,
      webApi: WebApi[F],
      reportingRoutes: ReportingHttpRoutes[F],
      prometheusReporter: NewPrometheusReporter
  )(implicit scheduler: Scheduler): Resource[F, server.Server[F]] =
    web.acquireHttpServer[F](
      nodeConf.apiServer.enableReporting,
      nodeConf.apiServer.host,
      nodeConf.apiServer.portHttp,
      prometheusReporter,
      nodeConf.apiServer.maxConnectionIdle,
      webApi,
      reportingRoutes
    )

  def adminWebApiServer[F[_]: ConcurrentEffect: Timer: NodeDiscovery: ConnectionsCell: RPConfAsk: Log](
      nodeConf: NodeConf,
      webApi: WebApi[F],
      adminWebApi: AdminWebApi[F],
      reportingRoutes: ReportingHttpRoutes[F]
  )(implicit scheduler: Scheduler): Resource[F, server.Server[F]] =
    web.acquireAdminHttpServer[F](
      nodeConf.apiServer.host,
      nodeConf.apiServer.portAdminHttp,
      nodeConf.apiServer.maxConnectionIdle,
      webApi,
      adminWebApi,
      reportingRoutes
    )

  def metricsInit[F[_]: Monixable: Sync](
      nodeConf: NodeConf,
      kamonConf: Config,
      prometheusReporter: NewPrometheusReporter
  ): Resource[F, Unit] = {
    def start(): Unit = {
      Kamon.reconfigure(kamonConf.withFallback(Kamon.config()))
      if (nodeConf.metrics.influxdb) Kamon.addReporter(new BatchInfluxDBReporter()).void()
      if (nodeConf.metrics.influxdbUdp) Kamon.addReporter(new UdpInfluxDBReporter()).void()
      if (nodeConf.metrics.prometheus) Kamon.addReporter(prometheusReporter).void()
      if (nodeConf.metrics.zipkin) Kamon.addReporter(new ZipkinReporter()).void()
      if (nodeConf.metrics.sigar) SystemMetrics.startCollecting()
    }
    // TODO: check new version of Kamon if supports custom effect
    def stop: Task[Unit] = Task.fromFuture(Kamon.stopAllReporters())

    Resource.make(Sync[F].delay(start()))(_ => stop.fromTask)
  }
}
