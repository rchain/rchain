package coop.rchain.rspace

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import coop.rchain.shared.Cell
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}

import scala.collection.concurrent.TrieMap

trait HotStore[F[_], C, P, A, K] {
  def getContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]]
  def putContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit]
  def removeContinuation(channels: Seq[C], index: Int): F[Unit]

  def getData(channel: C): F[Seq[Datum[A]]]
  def putDatum(channel: C, d: Datum[A]): F[Unit]
  def removeDatum(channel: C, index: Int): F[Unit]

  def getJoins(channel: C): F[Seq[Seq[C]]]
}

final case class Cache[C, P, A, K](
    continuations: TrieMap[Seq[C], Seq[WaitingContinuation[P, K]]] =
      TrieMap.empty[Seq[C], Seq[WaitingContinuation[P, K]]],
    data: TrieMap[C, Seq[Datum[A]]] = TrieMap.empty[C, Seq[Datum[A]]],
    joins: TrieMap[C, Seq[Seq[C]]] = TrieMap.empty[C, Seq[Seq[C]]]
)

private class InMemHotStore[F[_]: Sync, C, P, A, K](
    implicit S: Cell[F, Cache[C, P, A, K]],
    HR: HistoryReader[F, C, P, A, K]
) extends HotStore[F, C, P, A, K] {

  def getContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]] =
    for {
      state <- S.read
      res <- state.continuations.get(channels) match {
              case None =>
                for {
                  historyContinuations <- HR.getContinuations(channels)
                  _ <- S.flatModify { c =>
                        Sync[F]
                          .delay(c.continuations.putIfAbsent(channels, historyContinuations))
                          .map { _ =>
                            c
                          }
                      }
                } yield (historyContinuations)
              case Some(continuations) => Applicative[F].pure(continuations)
            }
    } yield (res)

  def putContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit] =
    for {
      continuations <- getContinuations(channels)
      _ <- S.flatModify { cache =>
            Sync[F].delay(cache.continuations.put(channels, wc +: continuations)).map(_ => cache)
          }
    } yield ()

  def removeContinuation(channels: Seq[C], index: Int): F[Unit] =
    for {
      continuations <- getContinuations(channels)
      _ <- S.flatModify { cache =>
            removeIndex(cache.continuations(channels), index) >>= { updated =>
              Sync[F]
                .delay {
                  cache.continuations.put(channels, updated)
                }
                .map(_ => cache)
            }
          }
    } yield ()

  def getData(channel: C): F[Seq[Datum[A]]] =
    for {
      state <- S.read
      res <- state.data.get(channel) match {
              case None =>
                for {
                  historyData <- HR.getData(channel)
                  _ <- S.flatModify { c =>
                        Sync[F].delay(c.data.putIfAbsent(channel, historyData)).map(_ => c)
                      }
                } yield (historyData)
              case Some(data) => Applicative[F].pure(data)
            }
    } yield (res)

  def putDatum(channel: C, datum: Datum[A]): F[Unit] =
    for {
      data <- getData(channel)
      _ <- S.flatModify { cache =>
            Sync[F].delay(cache.data.put(channel, datum +: data)).map(_ => cache)
          }
    } yield ()

  def removeDatum(channel: C, index: Int): F[Unit] =
    for {
      data <- getData(channel)
      _ <- S.flatModify { cache =>
            removeIndex(cache.data(channel), index) >>= { updated =>
              Sync[F]
                .delay {
                  cache.data.put(channel, updated)
                }
                .map(_ => cache)
            }
          }
    } yield ()

  def getJoins(channel: C): F[Seq[Seq[C]]] =
    for {
      state <- S.read
      res <- state.joins.get(channel) match {
              case None =>
                for {
                  historyJoins <- HR.getJoins(channel)
                  _ <- S.flatModify { c =>
                        Sync[F].delay(c.joins.putIfAbsent(channel, historyJoins)).map(_ => c)
                      }
                } yield (historyJoins)
              case Some(joins) => Applicative[F].pure(joins)
            }
    } yield (res)

  private def removeIndex[E](col: Seq[E], index: Int): F[Seq[E]] =
    if (col.isDefinedAt(index)) {
      val (l1, l2) = col splitAt index
      (l1 ++ (l2 tail)).pure[F]
    } else
      Sync[F].raiseError(
        new IndexOutOfBoundsException(
          s"Tried to remove index ${index} from a Vector of size ${col.size}"
        )
      )
}

object HotStore {

  def inMem[F[_]: Sync, C, P, A, K](
      implicit S: Cell[F, Cache[C, P, A, K]],
      HR: HistoryReader[F, C, P, A, K]
  ): HotStore[F, C, P, A, K] =
    new InMemHotStore[F, C, P, A, K]

}
