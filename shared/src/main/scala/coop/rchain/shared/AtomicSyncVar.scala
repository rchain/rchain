package coop.rchain.shared

import scala.concurrent.SyncVar
import scala.util.{Failure, Success, Try}

//A SyncVar which enforces the "consume/replace" model of
//atomic updates (same as Rholang's for/! pattern with a
//linear send).
class AtomicSyncVar[A](init: A) {
  private[this] val underlying: SyncVar[A] = new SyncVar()
  underlying.put(init)

  def get: A = underlying.get

  //Use case: capture an intermediate result in the process of updating the var.
  def mapAndUpdate[B](f: A => B, g: B => A): Either[Throwable, B] = {
    val curr = underlying.take()

    Try(f(curr))
      .flatMap(value => Try(value -> g(value)))
      .transform(
        { case (value, updated) => underlying.put(updated); Success(value) },
        (ex: Throwable) => { underlying.put(curr); Failure(ex) }
      )
      .toEither
  }

  def update(f: (A) => A): Either[Throwable, A] =
    mapAndUpdate(f, identity[A])
}
