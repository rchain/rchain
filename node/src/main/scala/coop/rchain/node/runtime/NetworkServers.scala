package coop.rchain.node.runtime

import cats.effect.{Async, Concurrent, ConcurrentEffect, ContextShift, IO, Resource, Sync, Timer}
import cats.syntax.all._
import com.typesafe.config.Config
import coop.rchain.casper.protocol.deploy.v1
import coop.rchain.casper.protocol.deploy.v1.DeployServiceFs2Grpc
import coop.rchain.casper.protocol.propose.v1.ProposeServiceFs2Grpc
import coop.rchain.comm.discovery.{KademliaHandleRPC, KademliaStore, NodeDiscovery}
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.comm.rp.HandleMessages
import coop.rchain.comm.transport.{GrpcTransportServer, TransportLayer}
import coop.rchain.comm.{discovery, RoutingMessage}
import coop.rchain.metrics.Metrics
import coop.rchain.node.api.{AdminWebApi, WebApi}
import coop.rchain.node.configuration.NodeConf
import coop.rchain.node.diagnostics.{
  BatchInfluxDBReporter,
  NewPrometheusReporter,
  UdpInfluxDBReporter
}
import coop.rchain.node.model.ReplFs2Grpc
import coop.rchain.node.web.ReportingRoutes.ReportingHttpRoutes
import coop.rchain.node.{api, web}
import coop.rchain.sdk.syntax.all._
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.concurrent.Queue
import io.grpc.{Metadata, Server}
import kamon.Kamon
import kamon.system.SystemMetrics
import kamon.zipkin.ZipkinReporter
import monix.execution.Scheduler
import org.http4s.server
import coop.rchain.shared.RChainScheduler._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object NetworkServers {

  /**
    * Creates API network service (gRPC and HTTP). Exposed as a [[Resource]] to gracefully shutdown.
    */
  // format: off
  def create[F[_]
    /* Execution */   : ConcurrentEffect: Timer: ContextShift
    /* Comm */        : TransportLayer: NodeDiscovery: KademliaStore: RPConfAsk: ConnectionsCell
    /* Diagnostics */ : Log: Metrics] // format: on
  (
      routingMessageQueue: Queue[F, RoutingMessage],
      grpcServices: GrpcServices[F],
      webApi: WebApi[F],
      adminWebApi: AdminWebApi[F],
      reportingRoutes: ReportingHttpRoutes[F],
      nodeConf: NodeConf,
      kamonConf: Config,
      grpcEC: ExecutionContext
  ): Resource[F, Unit] = {
    val GrpcServices(deploySrv, proposeSrv, replSrv) = grpcServices
    val host                                         = nodeConf.apiServer.host
    for {
      nodeAddress <- Resource.eval(RPConfAsk[F].ask.map(_.local.toAddress))

      intServer <- internalServer(nodeConf, replSrv, deploySrv, proposeSrv, grpcEC)
      _         <- Resource.eval(Log[F].info(s"Internal API server started at $host:${intServer.getPort}."))

      extServer    <- externalServer(nodeConf, deploySrv, grpcEC)
      extServerMsg = s"External API server started at $host:${extServer.getPort}."
      _            <- Resource.eval(Log[F].info(extServerMsg))

      _ <- protocolServer(nodeConf, routingMessageQueue)
      _ <- Resource.eval(Log[F].info(s"Listening for traffic on $nodeAddress."))

      discovery <- discoveryServer(nodeConf, grpcEC)
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

  def internalServer[F[_]: Concurrent: ConcurrentEffect: Log](
      nodeConf: NodeConf,
      replService: ReplFs2Grpc[F, Metadata],
      deployService: DeployServiceFs2Grpc[F, Metadata],
      proposeService: ProposeServiceFs2Grpc[F, Metadata],
      grpcEC: ExecutionContext
  ): Resource[F, Server] =
    api.acquireInternalServer[F](
      nodeConf.apiServer.host,
      nodeConf.apiServer.portGrpcInternal,
      grpcEC,
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

  def externalServer[F[_]: Concurrent: ConcurrentEffect: Log](
      nodeConf: NodeConf,
      deployService: v1.DeployServiceFs2Grpc[F, Metadata],
      grpcEC: ExecutionContext
  ): Resource[F, Server] =
    api.acquireExternalServer[F](
      nodeConf.apiServer.host,
      nodeConf.apiServer.portGrpcExternal,
      grpcEC,
      deployService,
      nodeConf.apiServer.grpcMaxRecvMessageSize.toInt,
      nodeConf.apiServer.keepAliveTime,
      nodeConf.apiServer.keepAliveTimeout,
      nodeConf.apiServer.permitKeepAliveTime,
      nodeConf.apiServer.maxConnectionIdle,
      nodeConf.apiServer.maxConnectionAge,
      nodeConf.apiServer.maxConnectionAgeGrace
    )

  def protocolServer[F[_]: Concurrent: ConcurrentEffect: TransportLayer: ConnectionsCell: RPConfAsk: Log: Metrics: Timer](
      nodeConf: NodeConf,
      routingMessageQueue: Queue[F, RoutingMessage]
  ): Resource[F, Unit] = {
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

  def discoveryServer[F[_]: Concurrent: ConcurrentEffect: KademliaStore: Log: Metrics](
      nodeConf: NodeConf,
      grpcEC: ExecutionContext
  ): Resource[F, Server] =
    discovery.acquireKademliaRPCServer(
      nodeConf.protocolServer.networkId,
      nodeConf.peersDiscovery.port,
      KademliaHandleRPC.handlePing[F],
      KademliaHandleRPC.handleLookup[F],
      grpcEC
    )

  def webApiServer[F[_]: ContextShift: ConcurrentEffect: Timer: NodeDiscovery: ConnectionsCell: RPConfAsk: Log](
      nodeConf: NodeConf,
      webApi: WebApi[F],
      reportingRoutes: ReportingHttpRoutes[F],
      prometheusReporter: NewPrometheusReporter
  ): Resource[F, server.Server[F]] =
    web.acquireHttpServer[F](
      nodeConf.apiServer.enableReporting,
      nodeConf.apiServer.host,
      nodeConf.apiServer.portHttp,
      prometheusReporter,
      nodeConf.apiServer.maxConnectionIdle,
      webApi,
      reportingRoutes
    )

  def adminWebApiServer[F[_]: ContextShift: ConcurrentEffect: Timer: NodeDiscovery: ConnectionsCell: RPConfAsk: Log](
      nodeConf: NodeConf,
      webApi: WebApi[F],
      adminWebApi: AdminWebApi[F],
      reportingRoutes: ReportingHttpRoutes[F]
  ): Resource[F, server.Server[F]] =
    web.acquireAdminHttpServer[F](
      nodeConf.apiServer.host,
      nodeConf.apiServer.portAdminHttp,
      nodeConf.apiServer.maxConnectionIdle,
      webApi,
      adminWebApi,
      reportingRoutes
    )

  def metricsInit[F[_]: Async](
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
    def stop: F[Unit] = Async[F].async { cb =>
      Kamon.stopAllReporters().onComplete {
        case Success(value) => cb(Right(value))
        case Failure(error) => cb(Left(error))
      }
    }

    Resource.make(Sync[F].delay(start()))(_ => stop)
  }
}
