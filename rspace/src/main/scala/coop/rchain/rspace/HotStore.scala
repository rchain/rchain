package coop.rchain.rspace

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.mtl.MonadState
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}

trait HotStore[F[_], C, P, A, K] {
  def getContinuations(channels: List[C]): F[List[WaitingContinuation[P, K]]]
  def putContinuation(channels: List[C], wc: WaitingContinuation[P, K]): F[Unit]

  //def getData(channel: C): F[List[Datum[A]]]
}

final case class Cache[C, P, K](
    continuations: Map[List[C], List[WaitingContinuation[P, K]]]
)

private class InMemHotStore[F[_]: Monad, C, P, A, K](
    )(
    implicit S: MonadState[F, Cache[C, P, K]],
    HR: HistoryReader[F, C, P, A, K]
) extends HotStore[F, C, P, A, K] {

  def getContinuations(channels: List[C]): F[List[WaitingContinuation[P, K]]] =
    for {
      cache <- S.get
      res <- cache.continuations.get(channels) match {
              case None                => HR.getContinuations(channels)
              case Some(continuations) => Applicative[F].pure(continuations)
            }
      updatedCache = cache.copy(cache.continuations + (channels -> res))
      _            <- S.set(updatedCache)
    } yield (res)

  def putContinuation(channels: List[C], wc: WaitingContinuation[P, K]): F[Unit] =
    for {
      continuations <- getContinuations(channels)
      _             <- S.modify(cache => cache.copy(cache.continuations.updated(channels, wc :: continuations)))
    } yield ()
}

object HotStore {

  def inMem[F[_]: Monad, C, P, A, K](
      implicit S: MonadState[F, Cache[C, P, K]],
      HR: HistoryReader[F, C, P, A, K]
  ): HotStore[F, C, P, A, K] =
    new InMemHotStore[F, C, P, A, K]

}
