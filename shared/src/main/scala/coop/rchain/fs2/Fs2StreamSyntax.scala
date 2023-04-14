package coop.rchain.fs2

import cats.effect.{Async, Ref, Temporal}
import cats.syntax.all._
import fs2.Stream
import fs2.Stream._

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{FiniteDuration, NANOSECONDS}

trait Fs2StreamSyntax {
  implicit final def sharedSyntaxFs2Stream[F[_], A](stream: Stream[F, A]): Fs2StreamOps[F, A] =
    new Fs2StreamOps[F, A](stream)

  implicit final def sharedSyntaxFs2StreamOfStreams[F[_], A](
      streams: Stream[F, Stream[F, A]]
  ): Fs2StreamOfStreamsOps[F, A] = new Fs2StreamOfStreamsOps[F, A](streams)
}

class Fs2StreamOps[F[_], A](
    // fs2 Stream extensions / syntax
    private val stream: Stream[F, A]
) {
  val availableProcessors = java.lang.Runtime.getRuntime.availableProcessors

  /**
    * Variant of [[Stream.parEvalMap]] with parallelism bound to number of processors.
    */
  def parEvalMapProcBounded[F2[x] >: F[x]: Async, B](f: A => F2[B]): Stream[F2, B] =
    stream.parEvalMap[F2, B](availableProcessors)(f)

  /**
    * Variant of [[Stream.parEvalMapUnordered]] with parallelism bound to number of processors.
    */
  def parEvalMapUnorderedProcBounded[F2[x] >: F[x]: Async, B](f: A => F2[B]): Stream[F2, B] =
    stream.parEvalMapUnordered[F2, B](availableProcessors)(f)

  /**
    * Variant of [[Stream.evalFilterAsync]] with parallelism bound to number of processors.
    */
  def evalFilterAsyncProcBounded[F2[x] >: F[x]: Async](f: A => F2[Boolean]): Stream[F2, A] =
    stream.evalFilterAsync[F2](availableProcessors)(f)

  /**
    * Variant of [[Stream.evalFilterAsync]] without keeping order of results.
    */
  def evalFilterAsyncUnordered[F2[x] >: F[x]: Async](
      maxConcurrent: Int
  )(f: A => F2[Boolean]): Stream[F2, A] =
    stream
      .parEvalMapUnordered[F2, Stream[F2, A]](maxConcurrent) { o =>
        f(o).map(if (_) Stream.emit(o) else Stream.empty)
      }
      .flatten

  /**
    * Variant of [[evalFilterAsyncUnordered]] with parallelism bound to number of processors.
    */
  def evalFilterAsyncUnorderedProcBounded[F2[x] >: F[x]: Async](
      f: A => F2[Boolean]
  ): Stream[F2, A] =
    evalFilterAsyncUnordered[F2](availableProcessors)(f)

  /**
    * Variation of [[Stream.takeWhile]] including ending element selected by predicate.

    * @param p predicate after which stream will terminate
    */
  def terminateAfter(p: A => Boolean): Stream[F, A] =
    stream.flatMap(x => if (p(x)) Stream(x.some, none) else Stream(x.some)).unNoneTerminate

  /**
    * Run action if stream is idle (not producing elements) for longer then timeout duration.
    *
    * @param action action to execute when timeout expires
    * @param timeout duration of idle period
    */
  def evalOnIdle[B](
      action: F[B],
      timeout: FiniteDuration
  )(implicit t: Temporal[F]): Stream[F, A] = {
    // Current time in nano seconds
    val nanoTime = Temporal[F].monotonic.map(_.toNanos)
    // Timeout in nano seconds
    val timeoutNano = timeout.toNanos

    Stream.eval(nanoTime.flatMap(n => Ref.of((n, true)))) flatMap { ref =>
      // Reset tracking time and mark as cleared
      val resetTimeRef = nanoTime.flatMap(n => ref.set((n, true)))

      // Reset tracking time and return sleep duration with elapsed flag
      def elapsedTimeRef(now: Long) = ref.modify {
        case (prev, cleared) =>
          // If tracking time is cleared, reduce sleep by elapsed idle time
          val idleElapsed = if (cleared) now - prev else 0
          val sleep       = timeoutNano - idleElapsed
          // If not cleared, timeout occurred
          val isTimeout = !cleared
          ((now, false), (sleep, isTimeout))
      }

      // Calculate sleep time and if timeout occurred
      val elapsed = for {
        now                    <- nanoTime
        elapsedRes             <- elapsedTimeRef(now)
        (sleepNano, isTimeout) = elapsedRes
        // Next check for timeout, min. 25 ms
        sleepDuration = FiniteDuration(sleepNano, TimeUnit.NANOSECONDS)
      } yield (sleepDuration, isTimeout)

      // Stream to execute action when timeout is reached, wait for next checking
      val nextStream = Stream.eval(elapsed) flatMap {
        case (sleep, isTimeout) =>
          Stream.eval(action).whenA(isTimeout) ++ Stream.eval(Temporal[F].sleep(sleep))
      }

      // On each element reset idle timer to current time | run next check recursively
      stream.evalTap(_ => resetTimeRef) concurrently nextStream.repeat
    }
  }

}

class Fs2StreamOfStreamsOps[F[_], A](
    // fs2 Stream of streams extensions / syntax
    private val streams: Stream[F, Stream[F, A]]
) {
  val availableProcessors = java.lang.Runtime.getRuntime.availableProcessors

  // TODO: [fs2 v3] Variant of [[Stream.NestedStreamOps.parJoin]] with parallelism bound to number of processors.
  /**
    * Variant of [[Stream.parJoin]] with parallelism bound to number of processors.
    */
  def parJoinProcBounded(implicit F: Async[F]): Stream[F, A] =
    streams.parJoin(availableProcessors)
}
