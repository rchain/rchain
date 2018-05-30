package coop.rchain.node.diagnostics

import cats._, cats.implicits._

import com.typesafe.config.ConfigValueFactory
import kamon._, kamon.prometheus._
import monix.eval.Task

object MetricsServer {
  def create[F[_]: Applicative](port: Int): F[MetricsServer] = new MetricsServer(port).pure[F]
}

class MetricsServer(port: Int) {

  def start: Task[Unit] = Task.delay {
    val kamonConfig = Kamon
      .config()
      .withValue("kamon.prometheus.embedded-server.port", ConfigValueFactory.fromAnyRef(port))
    Kamon.reconfigure(kamonConfig)
    Kamon.addReporter(new PrometheusReporter())
    Kamon.addReporter(new JmxReporter())
  }

  def stop(): Unit = Kamon.stopAllReporters()
}
