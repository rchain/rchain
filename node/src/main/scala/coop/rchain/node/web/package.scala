package coop.rchain.node

import cats.effect.{ConcurrentEffect, ExitCode, Timer}
import cats.syntax.all._
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.node.api.{AdminWebApi, WebApi}
import coop.rchain.node.diagnostics.NewPrometheusReporter
import coop.rchain.node.effects.EventConsumer
import coop.rchain.node.web.ReportingRoutes.ReportingHttpRoutes
import coop.rchain.shared.Log
import monix.execution.Scheduler
import org.http4s.HttpRoutes
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS

import scala.concurrent.duration.{DurationInt, FiniteDuration}

package object web {
  def aquireHttpServer[F[_]: ConcurrentEffect: Timer: RPConfAsk: NodeDiscovery: ConnectionsCell: EventConsumer: Log](
      reporting: Boolean,
      host: String = "0.0.0.0",
      httpPort: Int,
      prometheusReporter: NewPrometheusReporter,
      connectionIdleTimeout: FiniteDuration,
      webApiRoutes: WebApi[F],
      reportingRoutes: ReportingHttpRoutes[F]
  )(implicit scheduler: Scheduler): F[fs2.Stream[F, ExitCode]] =
    for {
      event              <- EventsInfo.service[F]
      reportingRoutesOpt = if (reporting) reportingRoutes else HttpRoutes.empty
      baseRoutes = Map(
        "/metrics"   -> CORS(NewPrometheusReporter.service[F](prometheusReporter)),
        "/version"   -> CORS(VersionInfo.service[F]),
        "/status"    -> CORS(StatusInfo.service[F]),
        "/ws/events" -> CORS(event),
        "/api"       -> CORS(WebApiRoutes.service[F](webApiRoutes) <+> reportingRoutesOpt)
      )
      // Legacy reporting routes
      extraRoutes = if (reporting)
        Map("/reporting" -> CORS(reportingRoutes))
      else
        Map.empty
      allRoutes = baseRoutes ++ extraRoutes
    } yield BlazeServerBuilder[F](scheduler)
      .bindHttp(httpPort, host)
      .withHttpApp(Router(allRoutes.toList: _*).orNotFound)
      .withIdleTimeout(connectionIdleTimeout)
      .withResponseHeaderTimeout(connectionIdleTimeout - 1.second)
      .serve

  def aquireAdminHttpServer[F[_]: ConcurrentEffect: Timer: EventConsumer](
      host: String = "0.0.0.0",
      httpPort: Int,
      connectionIdleTimeout: FiniteDuration,
      adminWebApiRoutes: AdminWebApi[F],
      reportingRoutes: ReportingHttpRoutes[F]
  )(implicit scheduler: Scheduler): F[fs2.Stream[F, ExitCode]] =
    for {
      event <- EventsInfo.service[F]
      baseRoutes = Map(
        "/api" -> CORS(AdminWebApiRoutes.service[F](adminWebApiRoutes) <+> reportingRoutes)
      )
    } yield BlazeServerBuilder[F](scheduler)
      .bindHttp(httpPort, host)
      .withHttpApp(Router(baseRoutes.toList: _*).orNotFound)
      .withResponseHeaderTimeout(connectionIdleTimeout - 1.second)
      .withIdleTimeout(connectionIdleTimeout)
      .serve
}
