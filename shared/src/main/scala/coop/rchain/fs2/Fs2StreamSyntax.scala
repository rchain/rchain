package coop.rchain.fs2

import java.util.concurrent.TimeUnit

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.shared.Time
import fs2.Stream

import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait Fs2StreamSyntax {
  implicit final def sharedSyntaxFs2Stream[F[_], A](stream: Stream[F, A]): Fs2StreamOps[F, A] =
    new Fs2StreamOps[F, A](stream)
}

class Fs2StreamOps[F[_], A](
    // fs2 Stream extensions / syntax
    private val stream: Stream[F, A]
) {

  /**
    * Variation of `takeWhile` including ending element selected by predicate.

    * @param p predicate after which stream will terminate
    */
  def terminateAfter(p: A => Boolean): Stream[F, A] =
    stream.flatMap(x => if (p(x)) Stream(x.some, none) else Stream(x.some)).unNoneTerminate

  /**
    * Run action if stream is idle (not producing elements) for longer then timeout.
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
      // Calculate elapsed time from last checking
      val elapsed = for {
        prevTime  <- ref.get
        now       = System.nanoTime
        duration  = now - prevTime
        isElapsed = duration > timeoutNano
        nextNano  = if (isElapsed) timeoutNano else timeoutNano - duration
        // Next check for timeout, min. 100ms
        nextDuration = FiniteDuration(nextNano, TimeUnit.NANOSECONDS).max(100.millis)
      } yield (isElapsed, nextDuration)
      // Stream to execute action when timeout is reached, wait for next checking
      val nextStream = Stream.eval(elapsed) flatMap {
        case (elapsed, next) =>
          Stream.eval(action).whenA(elapsed) ++ Stream.eval(Time[F].sleep(next)).drain
      }
      stream.evalTap(_ => ref.set(System.nanoTime)) concurrently nextStream.repeat
    }
}
