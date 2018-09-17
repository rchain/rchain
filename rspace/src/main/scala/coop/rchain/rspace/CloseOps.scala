package coop.rchain.rspace
import java.util.concurrent.atomic.AtomicInteger

trait CloseOps {
  import CloseOps._

  @volatile private[this] var isClosed = false

  private[this] val openedTransactions: AtomicInteger = new AtomicInteger(0)

  //in-memory store don't really need to count open transactions
  //and may not call countTransaction{Open|Close} methods
  protected def countTransactionOpen(): Unit = {
    failIfClosed()
    openedTransactions.incrementAndGet()
  }

  protected def countTransactionClose(): Unit =
    openedTransactions.decrementAndGet()

  def close(): Unit = {
    //do not allow new transactions to start
    isClosed = true
    //From one side - we are asked to close LMDB context, from other side we need to
    //wait until all opened transactions are finished (committed or aborted).
    //As a trade-off we're waiting up to closeTimeout, checking if
    //all transactions are done every closeSleepPeriod milliseconds,
    //and finally leaving this method (in this case SIGSEGV may be thrown by LMDB)
    (0L to closeTimeout by closeSleepPeriod).find(_ =>
      if (openedTransactions.get() == 0) {
        true
      } else {
        Thread.sleep(closeSleepPeriod)
        false
    })
  }

  def failIfClosed(): Unit =
    if (isClosed)
      throw new RSpaceClosedException()
}

private object CloseOps {
  //timeouts are in milliseconds
  private val closeSleepPeriod: Long = 100
  private val closeTimeout: Long     = 3000
}
