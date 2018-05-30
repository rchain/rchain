package coop.rchain.rspace
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.annotation.tailrec

case class StoreCounters(sizeOnDisk: Long,
                         dataEntries: Long,
                         consumesCount: Long,
                         consumeAvgMilliseconds: Double,
                         producesCount: Long,
                         produceAvgMilliseconds: Double)

/**
  * Conters rspace produce and consume calls.
  * Returns counted number of cycles and avg times
  * Uses BigIntegers to accumulate total times
  * w/o rounding errors
  *
  * Lock-free. Non wait-free.
  *
  * For percentiles here is an algorithm that will not drain
  * memory: http://www.cs.wustl.edu/~jain/papers/ftp/psqr.pdf,
  */
private[rspace] class StoreEventsCounter {
  private[this] val producesCount   = new AtomicLong
  private[this] val producesSumTime = new AtomicReference[BigInt](BigInt(0))

  private[this] val consumesCount   = new AtomicLong
  private[this] val consumesSumTime = new AtomicReference[BigInt](BigInt(0))

  // for nano-time pitfalls please read
  // https://shipilev.net/blog/2014/nanotrusting-nanotime/
  def registerProduce[T](f: => T): T = {
    val start = System.nanoTime()
    try {
      f
    } finally {
      val end  = System.nanoTime()
      val diff = Math.max(end - start, 0)
      //addAtomic can is times slower than incrementAndGet
      //run it first to minimize possible avg error
      addAtomic(producesSumTime, diff)
      producesCount.getAndIncrement()
    }
  }

  def registerConsume[T](f: => T): T = {
    val start = System.nanoTime()
    try {
      f
    } finally {
      val end  = System.nanoTime()
      val diff = Math.max(end - start, 0)
      //addAtomic can is times slower than incrementAndGet
      //run it first to minimize possible avg error
      addAtomic(consumesSumTime, BigInt(diff))
      consumesCount.getAndIncrement()
    }
  }

  @tailrec
  private[this] def addAtomic(value: AtomicReference[BigInt], add: BigInt): Unit = {
    val exInt = value.get
    if (!value.compareAndSet(exInt, exInt + add))
      addAtomic(value, add)
  }

  def createCounters(sizeOnDisk: Long, dataEntries: Long): StoreCounters = {
    //since producesCount and producesSumTime updated separately
    //we may have average a bit non-precise (lower) sometimes,
    //anyway that's better then waste speed on synchronization or locking
    val producesTotal = producesCount.get
    val produceAvgTime =
      if (producesTotal > 0)
        (BigDecimal(producesSumTime.get) / (producesTotal * 1000000)).toDouble
      else
        0

    val consumesTotal = consumesCount.get
    val consumeAvgTime =
      if (consumesTotal > 0)
        (BigDecimal(consumesSumTime.get) / (consumesTotal * 1000000)).toDouble
      else
        0

    StoreCounters(sizeOnDisk,
                  dataEntries,
                  consumesTotal,
                  consumeAvgTime,
                  producesTotal,
                  produceAvgTime)
  }

}
