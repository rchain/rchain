package coop.rchain.node

import cats.effect.Concurrent
import coop.rchain.casper.ReportingCasper
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.node.NodeRuntime.TaskEnv
import coop.rchain.node.api.WebApi
import coop.rchain.node.diagnostics.NewPrometheusReporter
import coop.rchain.node.effects.EventConsumer
import monix.eval.Task
import monix.execution.Scheduler
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS

package object web {
  def aquireHttpServer(
      reporting: Boolean,
      httpPort: Int,
      prometheusReporter: NewPrometheusReporter,
      reportingCasper: ReportingCasper[TaskEnv],
      webApiRoutes: WebApi[TaskEnv]
  )(
      implicit
      nodeDiscovery: NodeDiscovery[Task],
      connectionsCell: ConnectionsCell[Task],
      concurrent: Concurrent[Task],
      rPConfAsk: RPConfAsk[Task],
      consumer: EventConsumer[Task],
      mainScheduler: Scheduler
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
          WebApiRoutes.service[Task, TaskEnv](webApiRoutes)
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
      httpServerFiber <- BlazeServerBuilder[Task]
                          .bindHttp(httpPort, "0.0.0.0")
                          .withHttpApp(Router(allRoutes.toList: _*).orNotFound)
                          .resource
                          .use(_ => Task.never[Unit])
    } yield httpServerFiber
}
