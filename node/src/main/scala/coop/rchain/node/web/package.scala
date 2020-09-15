package coop.rchain.node

import cats.syntax.all._
import cats.effect.{Blocker, Concurrent, ContextShift}
import coop.rchain.casper.ReportingCasper
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.node.NodeRuntime.TaskEnv
import coop.rchain.node.api.WebApi
import coop.rchain.node.api.AdminWebApi
import coop.rchain.node.diagnostics.NewPrometheusReporter
import coop.rchain.node.effects.EventConsumer
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import com.http4s.rho.swagger.ui.SwaggerUi
import org.http4s.rho.swagger.SwaggerMetadata
import org.http4s.rho.swagger.models.{Contact, Info, License, Tag}

import scala.concurrent.duration.FiniteDuration
import org.http4s.rho.swagger.SwaggerSupport

package object web {
  val metaData = SwaggerMetadata(
    apiInfo = Info(
      title = "RChain node API",
      version = BuildInfo.version,
      license =
        License(name = "Apache 2.0", url = "http://www.apache.org/licenses/LICENSE-2.0.html").some,
      contact = Contact(
        name = "RChain Cooperative",
        url = "https://rchain.coop/".some,
        email = "info@rchain.coop".some
      ).some,
      description =
        "Build dApps for RChain, a decentralized, economically sustainable public compute infrastructure.".some
    ),
    tags = List(
      Tag(
        name = "blocks",
        description =
          "Find and explore blocks for establishing consensus on the state of the RhoVM.".some
      ),
      Tag(
        name = "rhovm",
        description = "Deploy Rholang code and explore the RhoVM tuple space.".some
      )
    ),
    basePath = Some("/api")
  )
  def aquireHttpServer(
      reporting: Boolean,
      host: String = "0.0.0.0",
      httpPort: Int,
      prometheusReporter: NewPrometheusReporter,
      reportingCasper: ReportingCasper[TaskEnv],
      webApiRoutes: WebApi[TaskEnv],
      connectionIdleTimeout: FiniteDuration
  )(
      implicit
      nodeDiscovery: NodeDiscovery[Task],
      connectionsCell: ConnectionsCell[Task],
      concurrent: Concurrent[Task],
      rPConfAsk: RPConfAsk[Task],
      consumer: EventConsumer[Task],
      scheduler: Scheduler,
      log: Log[Task]
  ): Task[Unit] =
    for {
      event <- EventsInfo.service[Task]
      baseRoutes = Map(
        "/metrics"   -> CORS(NewPrometheusReporter.service[Task](prometheusReporter)),
        "/version"   -> CORS(VersionInfo.service[Task]),
        "/status"    -> CORS(StatusInfo.service[Task]),
        "/ws/events" -> CORS(event),
        "/api" -> CORS({
          implicit val et = NodeRuntime.envToTask
          val swaggerUiRhoMiddleware =
            new SwaggerUi[Task].createRhoMiddleware(
              Blocker.liftExecutionContext(scheduler),
              swaggerRoutesInSwagger = true,
              swaggerMetadata = metaData
            )
          val ioSwagger = SwaggerSupport.apply[Task]
          WebApiRoutes
            .service[Task, TaskEnv](webApiRoutes, ioSwagger)
            .toRoutes(swaggerUiRhoMiddleware)
        })
      )
      extraRoutes = if (reporting)
        Map(
          "/reporting" -> CORS({
            implicit val et = NodeRuntime.envToTask
            ReportingRoutes.service[Task, TaskEnv](reportingCasper)
          })
        )
      else
        Map.empty
      allRoutes = baseRoutes ++ extraRoutes
      httpServerFiber <- BlazeServerBuilder[Task](scheduler)
                          .bindHttp(httpPort, host)
                          .withHttpApp(Router(allRoutes.toList: _*).orNotFound)
                          .withIdleTimeout(connectionIdleTimeout)
                          .resource
                          .use(_ => Task.never[Unit])
    } yield httpServerFiber

  def aquireAdminHttpServer(
      host: String = "0.0.0.0",
      httpPort: Int,
      adminWebApiRoutes: AdminWebApi[TaskEnv],
      connectionIdleTimeout: FiniteDuration
  )(
      implicit
      concurrent: Concurrent[Task],
      consumer: EventConsumer[Task],
      scheduler: Scheduler
  ): Task[Unit] =
    for {
      event <- EventsInfo.service[Task]
      baseRoutes = Map(
        "/api" -> CORS({
          implicit val et = NodeRuntime.envToTask
          AdminWebApiRoutes.service[Task, TaskEnv](adminWebApiRoutes)
        })
      )
      adminHttpServerFiber <- BlazeServerBuilder[Task](scheduler)
                               .bindHttp(httpPort, host)
                               .withHttpApp(Router(baseRoutes.toList: _*).orNotFound)
                               .withResponseHeaderTimeout(connectionIdleTimeout)
                               .withIdleTimeout(connectionIdleTimeout)
                               .resource
                               .use(_ => Task.never[Unit])
    } yield adminHttpServerFiber
}
