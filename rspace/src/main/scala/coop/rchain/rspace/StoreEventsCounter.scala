package coop.rchain.rspace
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import scala.collection.JavaConverters._

import scala.annotation.tailrec

case class StoreCount(count: Long, avgMilliseconds: Double, peakRate: Int, currentRate: Int)

case class StoreCounters(sizeOnDisk: Long,
                         dataEntries: Long,
                         consumesCount: StoreCount,
                         producesCount: StoreCount,
                         consumesCommCount: StoreCount,
                         producesCommCount: StoreCount,
                         installCommCount: StoreCount)

/**
  * Conters rspace produce and consume calls.
  * Returns counted number of cycles, avg times,
  * peak and current rate of events
  * Uses BigIntegers to accumulate total times
  * w/o rounding errors
  *
  * Lock-free. Non wait-free.
  *
  * For percentiles here is an algorithm that will not drain
  * memory: http://www.cs.wustl.edu/~jain/papers/ftp/psqr.pdf,
  */
private[rspace] class StoreEventsCounter(
    registrationIntervalNanoseconds: Long = 1L * 1000L * 1000000L) {

  private[this] case class SumCounter(sumTimeNanoseconds: BigInt, peakRate: Int, count: Long) {
    def add(diff: BigInt, rate: Int): SumCounter =
      SumCounter(sumTimeNanoseconds + diff, math.max(rate, peakRate), count + 1)

    def avgTimeMilliseconds: Double =
      if (count > 0)
        (BigDecimal(sumTimeNanoseconds) / (count * 1000000)).toDouble
      else
        0
  }

  final class Counter {
    private[this] val sumCounter: AtomicReference[SumCounter] =
      new AtomicReference[SumCounter](SumCounter(0, 0, 0))

    private[this] val eventsQueue: ConcurrentLinkedQueue[Long] = new ConcurrentLinkedQueue[Long]()

    def reset(): Unit = {
      sumCounter.set(SumCounter(0, 0, 0))
      eventsQueue.clear()
    }

    def add(start: Long, diff: BigInt): Unit = {
      eventsQueue.add(start)
      //no need to check for isEmpty, since: queue(0) - start === 0
      //speed over precision: due to possibility of out of order add,
      //we may sometime mistakenly include few events into current
      //second.
      while (start - eventsQueue.peek > registrationIntervalNanoseconds) {
        //remove head events outside of our interval
        eventsQueue.poll
      }
      //size of queue now == current rate
      addAtomic(diff, eventsQueue.size)
    }

    def count: Long = sumCounter.get.count

    def toStoreCount: StoreCount = {
      val exSum       = sumCounter.get
      val now         = nanoTime
      val currentRate = eventsQueue.asScala.count(x => now - x <= registrationIntervalNanoseconds)
      StoreCount(exSum.count, exSum.avgTimeMilliseconds, exSum.peakRate, currentRate)
    }

    @tailrec
    private[this] def addAtomic(diff: BigInt, rate: Int): Unit = {
      val exSum = sumCounter.get
      if (!sumCounter.compareAndSet(exSum, exSum.add(diff, rate)))
        addAtomic(diff, rate)
    }
  }

  protected def nanoTime: Long = System.nanoTime

  //produces
  private[this] val producesCounter = new Counter()

  def registerProduce[T](f: => T): T =
    register(producesCounter, f)

  private[rspace] def getProducesCount: Long = producesCounter.count

  //consumes
  private[this] val consumesCounter = new Counter()

  def registerConsume[T](f: => T): T =
    register(consumesCounter, f)

  private[rspace] def getConsumesCount: Long = consumesCounter.count

  //consumes Comm
  private[this] val consumesCommCounter = new Counter()

  def registerConsumeCommEvent[T](f: => T): T =
    register(consumesCommCounter, f)

  private[rspace] def getConsumesCommCount: Long = consumesCommCounter.count

  //produces Comm
  private[this] val producesCommCounter = new Counter()

  def registerProduceCommEvent[T](f: => T): T =
    register(producesCommCounter, f)

  private[rspace] def getProducesCommCount: Long = producesCommCounter.count

  //install Comm

  private[this] val installCommCounter = new Counter()

  def registerInstallCommEvent[T](f: => T): T =
    register(installCommCounter, f)

  private[rspace] def getInstallCommCount: Long = installCommCounter.count

  private[rspace] def reset(): Unit = {
    consumesCounter.reset
    producesCounter.reset
    consumesCommCounter.reset
    producesCommCounter.reset
    installCommCounter.reset
  }

  // for nano-time pitfalls please read
  // https://shipilev.net/blog/2014/nanotrusting-nanotime/
  def register[T](counter: Counter, f: => T): T = {
    val start = nanoTime
    try {
      f
    } finally {
      val end  = nanoTime
      val diff = Math.max(end - start, 0)
      counter.add(start, diff)
    }
  }

  def createCounters(sizeOnDisk: Long, dataEntries: Long): StoreCounters =
    StoreCounters(
      sizeOnDisk,
      dataEntries,
      consumesCounter.toStoreCount,
      producesCounter.toStoreCount,
      consumesCommCounter.toStoreCount,
      producesCommCounter.toStoreCount,
      installCommCounter.toStoreCount
    )
}
