package coop.rchain.comm.transport.buffer

import java.util

import scala.collection.mutable
import scala.math.ceil
import monix.execution.internal.atomic.UnsafeAccess
import monix.execution.internal.jctools.queues.MessagePassingQueue.Consumer
import monix.execution.internal.jctools.queues.atomic.MpscAtomicArrayQueue
import monix.execution.internal.jctools.queues.{
  MessagePassingQueue,
  MpscArrayQueue,
  MpscChunkedArrayQueue
}

/**
  * This code has been copied from the Monix codebase because unfortunately
  * these trait and object are private there.
  */
abstract class ConcurrentQueue[A] {
  def isEmpty: Boolean
  def poll(): A
  def offer(elem: A): Boolean
  def drain(buffer: mutable.Buffer[A], limit: Int): Unit
}

@SuppressWarnings(
  Array(
    "org.wartremover.warts.Return",
    "org.wartremover.warts.Var",
    "org.wartremover.warts.NonUnitStatements"
  )
)
object ConcurrentQueue {
  final val recommendedSize: Int = 1024

  final val lnOf2 = scala.math.log(2)

  private def log2(x: Double): Double =
    scala.math.log(x) / lnOf2

  private def nextPowerOf2(nr: Int): Int = {
    require(nr >= 0, "nr must be positive")
    val bit = ceil(log2(nr.toDouble))
    1 << (if (bit > 30) 30 else bit.toInt)
  }

  def limited[A](capacity: Int): ConcurrentQueue[A] = {
    val maxCapacity = math.max(4, nextPowerOf2(capacity))
    if (UnsafeAccess.IS_OPENJDK_COMPATIBLE) {
      new FromMessagePassingQueue[A](
        if (maxCapacity <= recommendedSize)
          new MpscArrayQueue[A](maxCapacity)
        else {
          val initialCapacity = math.min(recommendedSize, maxCapacity / 2)
          new MpscChunkedArrayQueue[A](initialCapacity, maxCapacity)
        }
      )
    } else {
      new FromAbstractQueue[A](new MpscAtomicArrayQueue[A](maxCapacity))
    }
  }

  private final class FromAbstractQueue[A](underlying: util.AbstractQueue[A])
      extends ConcurrentQueue[A] {

    def isEmpty: Boolean =
      underlying.isEmpty
    def offer(elem: A): Boolean =
      underlying.offer(elem)
    def poll(): A =
      underlying.poll()

    def drain(buffer: mutable.Buffer[A], limit: Int): Unit = {
      var fetched = 0

      while (fetched < limit) {
        val next = underlying.poll()
        if (next == null) return
        buffer += next
        fetched += 1
      }
    }
  }

  private final class FromMessagePassingQueue[A](underlying: MessagePassingQueue[A])
      extends ConcurrentQueue[A] {

    def isEmpty: Boolean =
      underlying.isEmpty
    def offer(elem: A): Boolean =
      underlying.relaxedOffer(elem)
    def poll(): A =
      underlying.relaxedPoll()

    def drain(buffer: mutable.Buffer[A], limit: Int): Unit = {
      val consumer: Consumer[A] = (e: A) => buffer += e
      underlying.drain(consumer, limit)
    }
  }
}
