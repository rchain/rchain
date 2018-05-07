package coop.rchain.node

import cats._
import cats.data._
import cats.implicits._

import coop.rchain.catscontrib._
import coop.rchain.p2p.effects.{Log, Metrics}

import monix.eval.Task
import monix.execution.Cancelable

object CpuUtilization {
  import javax.management.Attribute
  import javax.management.ObjectName
  import java.lang.management.ManagementFactory

  private val mbs  = ManagementFactory.getPlatformMBeanServer
  private val name = ObjectName.getInstance("java.lang:type=OperatingSystem")

  def currentCpuLoad: Double = {

    val list = mbs.getAttributes(name, Array[String]("ProcessCpuLoad"))

    if (list.isEmpty) {
      Double.NaN
    } else {
      val att   = list.get(0).asInstanceOf[Attribute]
      val value = att.getValue.asInstanceOf[Double]

      // usually takes a couple of seconds before we get real values
      if (value == -1.0) {
        Double.NaN
      } else {
        // returns a percentage value with 1 decimal point precision
        (value * 1000).toInt / 10.0
      }
    }
  }

  def reportProcessCpuLoad[F[_]: Monad: Capture: Metrics: Log]: F[Unit] =
    for {
      load <- Capture[F].capture(currentCpuLoad)
      _    <- Metrics[F].setGauge("cpu-load", load.toLong)
      _    <- Log[F].info(s"=========================> CPU-Load: $load")
    } yield ()
}
