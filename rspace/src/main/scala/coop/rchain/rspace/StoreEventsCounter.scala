package coop.rchain.rspace
import java.util.concurrent.atomic.AtomicReference
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

  final case class Counter(count: Long, sumTimeNanoseconds: BigInt) {
    def add(add: BigInt) =
      Counter(count + 1, sumTimeNanoseconds + add)

    def avgTimeMiliiseconds: Double =
      if (count > 0)
        (BigDecimal(sumTimeNanoseconds) / (count * 1000000)).toDouble
      else
        0
  }

  val zeroCounter = Counter(0, 0)

  private[this] val producesCounter = new AtomicReference[Counter](zeroCounter)
  private[this] val consumesCounter = new AtomicReference[Counter](zeroCounter)

  private[rspace] def reset(): Unit = {
    producesCounter.set(zeroCounter)
    consumesCounter.set(zeroCounter)
  }

  private[rspace] def getConsumesCount: Long = consumesCounter.get.count

  private[rspace] def getProducesCount: Long = producesCounter.get.count

  // for nano-time pitfalls please read
  // https://shipilev.net/blog/2014/nanotrusting-nanotime/
  def registerProduce[T](f: => T): T = {
    val start = System.nanoTime()
    try {
      f
    } finally {
      val end  = System.nanoTime()
      val diff = Math.max(end - start, 0)
      addAtomic(producesCounter, diff)
    }
  }

  def registerConsume[T](f: => T): T = {
    val start = System.nanoTime()
    try {
      f
    } finally {
      val end  = System.nanoTime()
      val diff = Math.max(end - start, 0)
      addAtomic(consumesCounter, diff)
    }
  }

  @tailrec
  private[this] def addAtomic(counter: AtomicReference[Counter], value: BigInt): Unit = {
    val exInt = counter.get
    if (!counter.compareAndSet(exInt, exInt.add(value)))
      addAtomic(counter, value)
  }

  def createCounters(sizeOnDisk: Long, dataEntries: Long): StoreCounters = {
    val consumesSnapshot = consumesCounter.get
    val producesSnapshot = producesCounter.get

    StoreCounters(sizeOnDisk,
                  dataEntries,
                  consumesSnapshot.count,
                  consumesSnapshot.avgTimeMiliiseconds,
                  producesSnapshot.count,
                  producesSnapshot.avgTimeMiliiseconds)
  }
}
