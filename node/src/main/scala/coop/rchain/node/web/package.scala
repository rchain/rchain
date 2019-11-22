package coop.rchain.node

import cats.Applicative
import cats.effect.{Concurrent, Sync}
import coop.rchain.casper.ReportingCasper
import coop.rchain.comm.discovery.NodeDiscovery
import coop.rchain.comm.rp.Connect.{ConnectionsCell, RPConfAsk}
import coop.rchain.node.NodeRuntime.TaskEnv
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
      reportingCasper: ReportingCasper[TaskEnv]
  )(
      implicit
      nodeDiscovery: NodeDiscovery[Task],
      connectionsCell: ConnectionsCell[Task],
      concurrent: Concurrent[Task],
      rPConfAsk: RPConfAsk[Task],
      consumer: EventConsumer[Task],
      scheduler: Scheduler
  ): Task[Unit] =
    for {
      event <- EventsInfo.service[Task]
      baseRoutes = Map(
        "/metrics"   -> CORS(NewPrometheusReporter.service[Task](prometheusReporter)),
        "/version"   -> CORS(VersionInfo.service[Task]),
        "/status"    -> CORS(StatusInfo.service[Task]),
        "/ws/events" -> CORS(event)
      )
      extraRoutes = if (reporting)
        Map(
          "/reporting" -> CORS(
            ReportingRoutes.service[Task, TaskEnv](reportingCasper)(
              Sync[Task],
              Applicative[TaskEnv],
              NodeRuntime.envToTask
            )
          )
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
