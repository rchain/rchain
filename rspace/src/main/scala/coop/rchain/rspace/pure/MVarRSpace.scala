package coop.rchain.rspace.pure
import cats.Monad
import cats.arrow.FunctionK
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.MVar
import cats.effect.internals.MVarConcurrent
import coop.rchain.rspace._
import cats.implicits._

import scala.collection.immutable.Seq

class MVarRSpace[F[_]: Concurrent: Monad, C, P, A, R, K](space: MVar[F, ISpace[C, P, A, R, K]])
    extends FISpace[F, C, P, A, R, K] {

  type Channel = MVar[F, ISpace[C, P, A, R, K]]

  val mvar: Channel = space

  def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, A, R],
      s: Sync[F]): F[Option[(K, Seq[R])]] =
    mvar.take.flatMap { space =>
      val r = space.consume(channels, patterns, continuation, persist).pure[F]
      mvar.put(space).flatMap(_ => r)
    }

  def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[P, A, R],
      s: Sync[F]): F[Option[(K, Seq[R])]] =
    mvar.take.flatMap { space =>
      val r = space.install(channels, patterns, continuation).pure[F]
      mvar.put(space).flatMap(_ => r)
    }

  def produce(channel: C, data: A, persist: Boolean)(implicit m: Match[P, A, R],
                                                     s: Sync[F]): F[Option[(K, Seq[R])]] =
    mvar.take.flatMap { space =>
      val r = space.produce(channel, data, persist).pure[F]
      mvar.put(space).flatMap(_ => r)
    }

  def createCheckpoint()(s: Sync[F]): F[Checkpoint] =
    mvar.take.flatMap { space =>
      val r = space.createCheckpoint().pure[F]
      mvar.put(space).flatMap(_ => r)
    }

  def reset(hash: Blake2b256Hash)(s: Sync[F]): F[Unit] =
    mvar.take.flatMap { space =>
      space.reset(hash).pure[F]
      mvar.put(space)
    }

  def close()(s: Sync[F]): F[Unit] =
    mvar.take.flatMap { space =>
      space.close().pure[F]
      mvar.put(space)
    }

}

object MVarRSpace {
  def create[M[_], C, P, A, R, K](space: ISpace[C, P, A, R, K])(
      implicit
      c: Concurrent[M],
      m: Monad[M]): M[MVarRSpace[M, C, P, A, R, K]] =
    MVar[M].of(space).map(data => new MVarRSpace[M, C, P, A, R, K](data))
}
