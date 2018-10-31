package coop.rchain.comm.transport.buffer

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.util.control.NonFatal

import monix.execution._
import monix.execution.Ack.{Continue, Stop}
import monix.execution.atomic.Atomic
import monix.execution.atomic.PaddingStrategy.LeftRight256
import monix.reactive.observers.Subscriber

/**
  * The purpose of the LimitedBuffer is to build a message queue with an
  * overflow strategy that informs the caller if the message has been enqueued
  * or dropped. Most of this code is copied from the Monix codebase,
  * specifically from monix.reactive.observers.buffers.DropNewBufferedSubscriber
  */
trait LimitedBuffer[A] {
  def pushNext(elem: A): Boolean
  def complete(): Unit
}

private final class DropNewBuffer[A](out: Subscriber[A], bufferSize: Int) extends LimitedBuffer[A] {

  require(bufferSize > 0, "bufferSize must be a strictly positive number")

  implicit val scheduler: Scheduler = out.scheduler
  private[this] val em              = out.scheduler.executionModel

  private[this] val itemsToPush =
    Atomic.withPadding(0, LeftRight256)

  private[this] val queue =
    ConcurrentQueue.limited[A](bufferSize)

  private[this] var downstreamIsComplete          = false
  private[this] var upstreamIsComplete            = false
  private[this] var lastIterationAck: Future[Ack] = _

  def pushNext(elem: A): Boolean =
    if (upstreamIsComplete || downstreamIsComplete) false
    else {
      if (queue.offer(elem)) {
        pushToConsumer()
        true
      } else false
    }

  def complete(): Unit =
    if (!upstreamIsComplete && !downstreamIsComplete) {
      upstreamIsComplete = true
      pushToConsumer()
    }

  private[this] def pushToConsumer(): Unit = {
    val currentNr = itemsToPush.getAndIncrement()

    // If a run-loop isn't started, then go, go, go!
    if (currentNr == 0) {
      // Starting the run-loop, as at this point we can be sure
      // that no other loop is active
      scheduler.execute(consumerRunLoop)
    }
  }

  private[this] val consumerRunLoop = new Runnable {
    def run(): Unit =
      // This lastIterationAck is also being set by the consumer-loop,
      // but it's important for the write to happen before `itemsToPush`,
      // to ensure its visibility
      fastLoop(lastIterationAck, 0, 0)

    private final def signalNext(next: A): Future[Ack] =
      try {
        val ack = out.onNext(next)
        // Tries flattening the Future[Ack] to a
        // synchronous value
        if (ack == Continue || ack == Stop)
          ack
        else
          ack.value match {
            case Some(Success(success)) =>
              success
            case Some(Failure(ex)) =>
              signalError(ex)
              Stop
            case None =>
              ack
          }
      } catch {
        case ex if NonFatal(ex) =>
          signalError(ex)
          Stop
      }

    private final def signalComplete(): Unit =
      try out.onComplete()
      catch {
        case ex if NonFatal(ex) =>
          scheduler.reportFailure(ex)
      }

    private final def signalError(ex: Throwable): Unit =
      try out.onError(ex)
      catch {
        case err if NonFatal(err) =>
          scheduler.reportFailure(err)
      }

    private def goAsync(next: A, ack: Future[Ack], processed: Int): Unit =
      ack.onComplete {
        case Success(Continue) =>
          val nextAck   = signalNext(next)
          val isSync    = ack == Continue || ack == Stop
          val nextFrame = if (isSync) em.nextFrameIndex(0) else 0
          fastLoop(nextAck, processed + 1, nextFrame)

        case Success(Stop) =>
          // ending loop
          downstreamIsComplete = true

        case Failure(ex) =>
          // ending loop
          downstreamIsComplete = true
          signalError(ex)
      }

    private def fastLoop(prevAck: Future[Ack], lastProcessed: Int, startIndex: Int): Unit = {
      var ack              = if (prevAck == null) Continue else prevAck
      var isFirstIteration = ack == Continue
      var processed        = lastProcessed
      var nextIndex        = startIndex

      while (!downstreamIsComplete) {
        var streamErrors = true
        try {
          // The `processed` count is only for counting things processed
          // from the queue, but not overflow messages, as these are
          // not pushed to the queue - so we keep track of what to add
          val next = queue.poll()

          // Threshold after which we are no longer allowed to
          // stream errors downstream if they happen
          streamErrors = false

          if (next != null) {
            if (nextIndex > 0 || isFirstIteration) {
              isFirstIteration = false

              ack match {
                case Continue =>
                  ack = signalNext(next)
                  if (ack == Stop) {
                    // ending loop
                    downstreamIsComplete = true
                    return
                  } else {
                    val isSync = ack == Continue
                    nextIndex = if (isSync) em.nextFrameIndex(nextIndex) else 0
                    processed += 1
                  }

                case Stop =>
                  // ending loop
                  downstreamIsComplete = true
                  return

                case _ =>
                  goAsync(next, ack, processed)
                  return
              }
            } else {
              goAsync(next, ack, processed)
              return
            }
          } else if (upstreamIsComplete) {
            // Race-condition check, but if upstreamIsComplete=true is
            // visible, then the queue should be fully published because
            // there's a clear happens-before relationship between
            // queue.offer() and upstreamIsComplete=true
            if (queue.isEmpty) {
              // ending loop
              downstreamIsComplete = true

              signalComplete()
              return
            }
          } else {
            // Given we are writing in `itemsToPush` before this
            // assignment, it means that writes will not get reordered,
            // so when we observe that itemsToPush is zero on the
            // producer side, we will also have the latest lastIterationAck
            lastIterationAck = ack
            val remaining = itemsToPush.decrementAndGet(processed)

            processed = 0
            // if the queue is non-empty (i.e. concurrent modifications
            // just happened) then continue loop, otherwise stop
            if (remaining <= 0) return
          }
        } catch {
          case ex if NonFatal(ex) =>
            if (streamErrors) {
              // ending loop
              downstreamIsComplete = true
              signalError(ex)
            } else {
              scheduler.reportFailure(ex)
            }
        }
      }
    }
  }
}
