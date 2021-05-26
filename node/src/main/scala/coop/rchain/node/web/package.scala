package coop.rchain.node

import cats.effect.{Async, ConcurrentEffect, ExitCode, Timer}
import cats.syntax.all._
import cats.~>
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.{ReportingCasper, SafetyOracle}
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.metrics.{Metrics, Span}
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
  // TODO: Temp until web API is refactored with one effect type.
  def natId[F[_]]: F ~> F = λ[F ~> F](x => x)

  def aquireHttpServer[F[_]: ConcurrentEffect: Timer: RPConfAsk: NodeDiscovery: ConnectionsCell: EventConsumer: Log](
      reporting: Boolean,
      host: String = "0.0.0.0",
      httpPort: Int,
      prometheusReporter: NewPrometheusReporter,
      webApiRoutes: WebApi[F],
      connectionIdleTimeout: FiniteDuration,
      reportingRoutes: ReportingHttpRoutes[F]
  )(implicit scheduler: Scheduler): F[fs2.Stream[F, ExitCode]] =
    for {
      event <- EventsInfo.service[F]
      baseRoutes = Map(
        "/metrics"   -> CORS(NewPrometheusReporter.service[F](prometheusReporter)),
        "/version"   -> CORS(VersionInfo.service[F]),
        "/status"    -> CORS(StatusInfo.service[F]),
        "/ws/events" -> CORS(event),
        "/api" -> CORS({
          implicit val n = natId[F]
          WebApiRoutes.service[F, F](webApiRoutes)
        })
      )
      extraRoutes = if (reporting)
        Map(
          "/reporting" -> CORS(reportingRoutes)
        )
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
      adminWebApiRoutes: AdminWebApi[F],
      connectionIdleTimeout: FiniteDuration
  )(implicit scheduler: Scheduler): F[fs2.Stream[F, ExitCode]] =
    for {
      event <- EventsInfo.service[F]
      baseRoutes = Map(
        "/api" -> CORS({
          implicit val n = natId[F]
          AdminWebApiRoutes.service[F, F](adminWebApiRoutes)
        })
      )
    } yield BlazeServerBuilder[F](scheduler)
      .bindHttp(httpPort, host)
      .withHttpApp(Router(baseRoutes.toList: _*).orNotFound)
      .withResponseHeaderTimeout(connectionIdleTimeout - 1.second)
      .withIdleTimeout(connectionIdleTimeout)
      .serve
}
