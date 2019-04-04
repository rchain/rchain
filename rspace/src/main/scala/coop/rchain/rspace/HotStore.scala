package coop.rchain.rspace

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import coop.rchain.shared.Cell
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}

import scala.collection.concurrent.TrieMap

trait HotStore[F[_], C, P, A, K] {
  def getContinuations(channels: List[C]): F[List[WaitingContinuation[P, K]]]
  def putContinuation(channels: List[C], wc: WaitingContinuation[P, K]): F[Unit]

  def getData(channel: C): F[List[Datum[A]]]
  def putDatum(channel: C, d: Datum[A]): F[Unit]

  def getJoins(channel: C): F[List[List[C]]]
}

final case class Cache[C, P, A, K](
    continuations: TrieMap[List[C], List[WaitingContinuation[P, K]]] =
      TrieMap.empty[List[C], List[WaitingContinuation[P, K]]],
    data: TrieMap[C, List[Datum[A]]] = TrieMap.empty[C, List[Datum[A]]],
    joins: TrieMap[C, List[List[C]]] = TrieMap.empty[C, List[List[C]]]
)

private class InMemHotStore[F[_]: Sync, C, P, A, K](
    implicit S: Cell[F, Cache[C, P, A, K]],
    HR: HistoryReader[F, C, P, A, K]
) extends HotStore[F, C, P, A, K] {

  def getContinuations(channels: List[C]): F[List[WaitingContinuation[P, K]]] =
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

  def putContinuation(channels: List[C], wc: WaitingContinuation[P, K]): F[Unit] =
    for {
      continuations <- getContinuations(channels)
      _ <- S.flatModify { cache =>
            Sync[F].delay(cache.continuations.put(channels, wc :: continuations)).map(_ => cache)
          }
    } yield ()

  def getData(channel: C): F[List[Datum[A]]] =
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
            Sync[F].delay(cache.data.put(channel, datum :: data)).map(_ => cache)
          }
    } yield ()

  def getJoins(channel: C): F[List[List[C]]] =
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
}

object HotStore {

  def inMem[F[_]: Sync, C, P, A, K](
      implicit S: Cell[F, Cache[C, P, A, K]],
      HR: HistoryReader[F, C, P, A, K]
  ): HotStore[F, C, P, A, K] =
    new InMemHotStore[F, C, P, A, K]

}
