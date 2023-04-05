package coop.rchain.shared

import cats.effect.{ContextShift, IO}

import java.util.concurrent.{Executors, ThreadFactory}
import java.util.concurrent.atomic.AtomicLong

object RChainScheduler {
  implicit val mainEC                 = scala.concurrent.ExecutionContext.Implicits.global
  implicit val csIO: ContextShift[IO] = IO.contextShift(mainEC)
  val rholangEC                       = mainEC
  implicit val timer                  = IO.timer(mainEC)

  val ioScheduler = Executors.newCachedThreadPool(new ThreadFactory {
    private val counter = new AtomicLong(0L)

    def newThread(r: Runnable) = {
      val th = new Thread(r)
      th.setName(
        "io-thread-" +
          counter.getAndIncrement.toString
      )
      th.setDaemon(true)
      th
    }
  })
}
