package coop.rchain.shared

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{Executors, ThreadFactory}

object RChainScheduler {
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
