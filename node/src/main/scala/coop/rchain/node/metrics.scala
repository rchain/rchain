package coop.rchain.node

import cats._, cats.data._, cats.implicits._
import kamon.prometheus._
import kamon._
import monix.eval.Task
import com.typesafe.config.ConfigValueFactory

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
  }

  def stop(): Unit = Kamon.stopAllReporters()
}
