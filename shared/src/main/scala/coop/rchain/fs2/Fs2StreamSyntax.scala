package coop.rchain.fs2

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.shared.Time
import fs2.Stream
import fs2.Stream._

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{DurationInt, FiniteDuration}

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
  def parEvalMapProcBounded[F2[x] >: F[x]: Concurrent, B](f: A => F2[B]): Stream[F2, B] =
    stream.parEvalMap[F2, B](availableProcessors)(f)

  /**
    * Variant of [[Stream.parEvalMapUnordered]] with parallelism bound to number of processors.
    */
  def parEvalMapUnorderedProcBounded[F2[x] >: F[x]: Concurrent, B](f: A => F2[B]): Stream[F2, B] =
    stream.parEvalMapUnordered[F2, B](availableProcessors)(f)

  /**
    * Variant of [[Stream.evalFilterAsync]] with parallelism bound to number of processors.
    */
  def evalFilterAsyncProcBounded[F2[x] >: F[x]: Concurrent, B](f: A => F2[Boolean]): Stream[F2, A] =
    stream.evalFilterAsync[F2](availableProcessors)(f)

  /**
    * Variation of [[Stream.takeWhile]] including ending element selected by predicate.

    * @param p predicate after which stream will terminate
    */
  def terminateAfter(p: A => Boolean): Stream[F, A] =
    stream.flatMap(x => if (p(x)) Stream(x.some, none) else Stream(x.some)).unNoneTerminate

  /**
    * Run action if stream is idle (not producing elements) for longer then timeout duration.
    *
    * @param timeout duration of idle period
    * @param action action to execute when timeout expires
    */
  def onIdle[B](
      timeout: FiniteDuration,
      action: F[B]
  )(implicit c: Concurrent[F], t: Time[F]): Stream[F, A] =
    Stream.eval(Ref.of(System.nanoTime)) flatMap { ref =>
      val timeoutNano = timeout.toNanos

      // Reset tracking idle time to current time
      val resetTimeRef = Stream.eval(ref.set(System.nanoTime))

      // Calculate elapsed time from last checking
      val elapsed = for {
        prevTime  <- ref.get
        now       = System.nanoTime
        duration  = now - prevTime
        isElapsed = duration > timeoutNano
        nextNano  = if (isElapsed) timeoutNano else timeoutNano - duration
        // Next check for timeout, min. 25 ms
        nextDuration = FiniteDuration(nextNano, TimeUnit.NANOSECONDS).max(25.millis)
      } yield (isElapsed, nextDuration)

      // Stream to execute action when timeout is reached, wait for next checking
      val nextStream = resetTimeRef.drain ++ Stream.eval(elapsed) flatMap {
        case (elapsed, next) =>
          Stream.eval(action).whenA(elapsed) ++ Stream.eval(Time[F].sleep(next)).drain
      }

      // On each element reset idle timer to current time | run next check recursively
      stream.flatTap(_ => resetTimeRef) concurrently nextStream.repeat
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
  def parJoinProcBounded(implicit F: Concurrent[F]): Stream[F, A] =
    streams.parJoin(availableProcessors)
}
