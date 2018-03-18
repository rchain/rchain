package coop.rchain.node

import kamon.prometheus._
import kamon._
import scala.util.control.NonFatal

final case class MetricsServer() {
  val reporter = new PrometheusReporter()

  Kamon.addReporter(reporter)

  /*
   * Small interface to demonstrate the External Node API.
   */

  def start(): Unit =
    try {
      reporter.start
    } catch {
      case NonFatal(_) => ()
    }

  def stop(): Unit =
    reporter.stop
}
