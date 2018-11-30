package coop.rchain.shared

import cats.Id

import monix.execution.UncaughtExceptionReporter

object UncaughtExceptionLogger extends UncaughtExceptionReporter {
  private implicit val logSource: LogSource = LogSource(this.getClass)
  private val log: Log[Id]                  = Log.logId

  def reportFailure(ex: scala.Throwable): Unit =
    log.error(s"Uncaught Exception : ${ex.getMessage}", ex)
}
