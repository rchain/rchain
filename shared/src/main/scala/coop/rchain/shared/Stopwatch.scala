package coop.rchain.shared

import cats.effect.Sync
import cats.syntax.all._

import scala.concurrent.duration.{Duration, FiniteDuration}

object Stopwatch {

  // TODO: this is temporary solution to measure and log duration.
  def time[F[_]: Sync, A](log: String => F[Unit])(tag: String)(block: => F[A]): F[A] =
    for {
      t0 <- Sync[F].delay(System.nanoTime)
      a  <- block
      t1 = System.nanoTime
      m  = Duration.fromNanos(t1 - t0)
      _  <- log(s"$tag [${showTime(m)}]")
    } yield a

  def showTime(d: FiniteDuration): String = {
    val ns   = 1d
    val ms   = 1e6 * ns
    val sec  = 1000 * ms
    val min  = 60 * sec
    val hour = 60 * min
    val m    = d.toNanos
    if (m >= hour) s"${m / hour} hour"
    else if (m >= min) s"${m / min} min"
    else if (m >= sec) s"${m / sec} sec"
    else if (m >= ms) s"${m / ms} ms"
    else s"${m / 1e6d} ms"
  }

}
