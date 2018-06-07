package coop.rchain.rspace
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

case class StoreCount(count: Long, avgMilliseconds: Double)

case class StoreCounters(sizeOnDisk: Long,
                         dataEntries: Long,
                         consumesCount: StoreCount,
                         producesCount: StoreCount,
                         consumesCommCount: StoreCount,
                         producesCommCount: StoreCount,
                         installCommCount: StoreCount)

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

    def toStoreCount: StoreCount =
      StoreCount(count, avgTimeMiliiseconds)
  }

  val zeroCounter = Counter(0, 0)

  //produces
  private[this] val producesCounter = new AtomicReference[Counter](zeroCounter)

  def registerProduce[T](f: => T): T =
    register(producesCounter, f)

  private[rspace] def getProducesCount: Long = producesCounter.get.count

  //consumes
  private[this] val consumesCounter = new AtomicReference[Counter](zeroCounter)

  def registerConsume[T](f: => T): T =
    register(consumesCounter, f)

  private[rspace] def getConsumesCount: Long = consumesCounter.get.count

  //consumes Comm
  private[this] val consumesCommCounter = new AtomicReference[Counter](zeroCounter)

  def registerConsumeCommEvent[T](f: => T): T =
    register(consumesCommCounter, f)

  private[rspace] def getConsumesCommCount: Long = consumesCommCounter.get.count

  //produces Comm
  private[this] val producesCommCounter = new AtomicReference[Counter](zeroCounter)

  def registerProduceCommEvent[T](f: => T): T =
    register(producesCommCounter, f)

  private[rspace] def getProducesCommCount: Long = producesCommCounter.get.count

  //install Comm

  private[this] val installCommCounter = new AtomicReference[Counter](zeroCounter)

  def registerInstallCommEvent[T](f: => T): T =
    register(installCommCounter, f)

  private[rspace] def getInstallCommCount: Long = installCommCounter.get.count

  private[rspace] def reset(): Unit = {
    consumesCounter.set(zeroCounter)
    producesCounter.set(zeroCounter)
    consumesCommCounter.set(zeroCounter)
    producesCommCounter.set(zeroCounter)
    installCommCounter.set(zeroCounter)
  }

  // for nano-time pitfalls please read
  // https://shipilev.net/blog/2014/nanotrusting-nanotime/
  def register[T](counter: AtomicReference[Counter], f: => T): T = {
    val start = System.nanoTime()
    try {
      f
    } finally {
      val end  = System.nanoTime()
      val diff = Math.max(end - start, 0)
      addAtomic(counter, diff)
    }
  }

  @tailrec
  private[this] def addAtomic(counter: AtomicReference[Counter], value: BigInt): Unit = {
    val exInt = counter.get
    if (!counter.compareAndSet(exInt, exInt.add(value)))
      addAtomic(counter, value)
  }

  def createCounters(sizeOnDisk: Long, dataEntries: Long): StoreCounters =
    StoreCounters(
      sizeOnDisk,
      dataEntries,
      consumesCounter.get.toStoreCount,
      producesCounter.get.toStoreCount,
      consumesCommCounter.get.toStoreCount,
      producesCommCounter.get.toStoreCount,
      installCommCounter.get.toStoreCount
    )
}
