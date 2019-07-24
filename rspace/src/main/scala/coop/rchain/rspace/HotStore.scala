package coop.rchain.rspace

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import coop.rchain.catscontrib.mtl.implicits._
import coop.rchain.catscontrib.seq._
import coop.rchain.shared.Cell
import coop.rchain.shared.MapOps._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.Serialize._
import coop.rchain.rspace.trace.Consume
import scodec.Codec

import scala.collection.SortedSet
import scala.collection.concurrent.TrieMap

final case class Snapshot[C, P, A, K](private[rspace] val cache: Cache[C, P, A, K])

trait HotStore[F[_], C, P, A, K] {
  def getContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]]
  def putContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit]
  def installContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit]
  def removeContinuation(channels: Seq[C], index: Int): F[Unit]

  def getData(channel: C): F[Seq[Datum[A]]]
  def putDatum(channel: C, d: Datum[A]): F[Unit]
  def removeDatum(channel: C, index: Int): F[Unit]

  def getJoins(channel: C): F[Seq[Seq[C]]]
  def putJoin(channel: C, join: Seq[C]): F[Unit]
  def installJoin(channel: C, join: Seq[C]): F[Unit]
  def removeJoin(channel: C, join: Seq[C]): F[Unit]

  def changes(): F[Seq[HotStoreAction]]
  def toMap: F[Map[Seq[C], Row[P, A, K]]]
  def snapshot(): F[Snapshot[C, P, A, K]]
}

final case class Cache[C, P, A, K](
    continuations: TrieMap[Seq[C], Seq[WaitingContinuation[P, K]]] =
      TrieMap.empty[Seq[C], Seq[WaitingContinuation[P, K]]],
    installedContinuations: TrieMap[Seq[C], WaitingContinuation[P, K]] =
      TrieMap.empty[Seq[C], WaitingContinuation[P, K]],
    data: TrieMap[C, Seq[Datum[A]]] = TrieMap.empty[C, Seq[Datum[A]]],
    joins: TrieMap[C, Seq[Seq[C]]] = TrieMap.empty[C, Seq[Seq[C]]],
    installedJoins: TrieMap[C, Seq[Seq[C]]] = TrieMap.empty[C, Seq[Seq[C]]]
) {
  def snapshot(): Snapshot[C, P, A, K] =
    Snapshot(
      this.copy(
        continuations = this.continuations.snapshot(),
        installedContinuations = this.installedContinuations.snapshot(),
        data = this.data.snapshot(),
        joins = this.joins.snapshot(),
        installedJoins = this.installedJoins.snapshot()
      )
    )
}

private class InMemHotStore[F[_]: Sync, C, P, A, K](
    implicit S: Cell[F, Cache[C, P, A, K]],
    HR: HistoryReader[F, C, P, A, K],
    ck: Codec[K]
) extends HotStore[F, C, P, A, K] {

  implicit val codec = fromCodec(ck)

  def snapshot(): F[Snapshot[C, P, A, K]] = S.read.map(_.snapshot())

  def getContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]] =
    for {
      cached <- internalGetContinuations(channels)
      state  <- S.read
      result = state.installedContinuations.get(channels) ++: cached
    } yield result

  private[this] def internalGetContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]] =
    for {
      state <- S.read
      res <- state.continuations.get(channels) match {
              case None =>
                for {
                  historyContinuations <- HR.getContinuations(channels)
                  _ <- S.flatModify { cache =>
                        Sync[F]
                          .delay(cache.continuations.putIfAbsent(channels, historyContinuations))
                          .as(cache)
                      }
                } yield (historyContinuations)
              case Some(continuations) => Applicative[F].pure(continuations)
            }
    } yield (res)

  def putContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit] =
    for {
      continuations <- internalGetContinuations(channels)
      _ <- S.flatModify { cache =>
            Sync[F].delay(cache.continuations.put(channels, wc +: continuations)).as(cache)
          }
    } yield ()

  def installContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit] = S.flatModify {
    cache =>
      Sync[F].delay(cache.installedContinuations.put(channels, wc)).map(_ => cache)
  }

  def removeContinuation(channels: Seq[C], index: Int): F[Unit] =
    for {
      continuations <- getContinuations(channels)
      cache         <- S.read
      installed     = cache.installedContinuations.get(channels)
      _ <- if (installed.isDefined && index == 0)
            Sync[F].raiseError {
              new IllegalArgumentException("Attempted to remove an installed continuation")
            } else
            S.flatModify { cache =>
              removeIndex(
                continuations,
                index
              ) >>= { updated =>
                Sync[F]
                  .delay {
                    installed match {
                      case Some(_) => cache.continuations.put(channels, updated tail)
                      case None    => cache.continuations.put(channels, updated)
                    }
                  }
                  .as(cache)
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
                  _ <- S.flatModify { cache =>
                        Sync[F].delay(cache.data.putIfAbsent(channel, historyData)).as(cache)
                      }
                } yield (historyData)
              case Some(data) => Applicative[F].pure(data)
            }
    } yield (res)

  def putDatum(channel: C, datum: Datum[A]): F[Unit] =
    for {
      data <- getData(channel)
      _ <- S.flatModify { cache =>
            Sync[F].delay(cache.data.put(channel, datum +: data)).as(cache)
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
                .as(cache)
            }
          }
    } yield ()

  def getJoins(channel: C): F[Seq[Seq[C]]] =
    for {
      cached <- internalGetJoins(channel)
      state  <- S.read
    } yield (state.installedJoins.get(channel).getOrElse(Seq.empty) ++: cached)

  private[this] def internalGetJoins(channel: C): F[Seq[Seq[C]]] =
    for {
      state <- S.read
      res <- state.joins.get(channel) match {
              case None =>
                for {
                  historyJoins <- HR.getJoins(channel)
                  _ <- S.flatModify { cache =>
                        Sync[F].delay(cache.joins.putIfAbsent(channel, historyJoins)).as(cache)
                      }
                } yield (historyJoins)
              case Some(joins) => Applicative[F].pure(joins)
            }
    } yield (res)

  def putJoin(channel: C, join: Seq[C]): F[Unit] =
    for {
      joins <- getJoins(channel)
      _ <- if (!joins.contains(join)) S.flatModify { cache =>
            Sync[F].delay(cache.joins.put(channel, join +: joins)).as(cache)
          } else Applicative[F].unit
    } yield ()

  def installJoin(channel: C, join: Seq[C]): F[Unit] = S.flatModify { cache =>
    Sync[F]
      .delay {
        val installed = cache.installedJoins.get(channel).getOrElse(Seq.empty)

        if (!installed.contains(join))
          cache.installedJoins
            .put(channel, join +: installed)
      }
      .map(_ => cache)
  }

  def removeJoin(channel: C, join: Seq[C]): F[Unit] =
    for {
      joins         <- internalGetJoins(channel)
      continuations <- getContinuations(join)
      _ <- if (continuations.isEmpty) S.flatModify { cache =>
            val index = cache.joins(channel).indexOf(join)
            if (index != -1) removeIndex(cache.joins(channel), index) >>= { updated =>
              Sync[F]
                .delay {
                  cache.joins.put(channel, updated)
                }
                .as(cache)
            } else cache.pure[F]
          } else Applicative[F].unit
    } yield ()

  def changes(): F[Seq[HotStoreAction]] =
    for {
      cache <- S.read
      continuations = (cache.continuations
        .readOnlySnapshot()
        .map {
          case (k, v) if (v.isEmpty) => DeleteContinuations(k)
          case (k, v)                => InsertContinuations(k, v)
        })
        .toVector
      data = (cache.data
        .readOnlySnapshot()
        .map {
          case (k, v) if (v.isEmpty) => DeleteData(k)
          case (k, v)                => InsertData(k, v)
        })
        .toVector
      joins = (cache.joins
        .readOnlySnapshot()
        .map {
          case (k, v) if (v.isEmpty) => DeleteJoins(k)
          case (k, v)                => InsertJoins(k, v)
        })
        .toVector
    } yield (continuations ++ data ++ joins)

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

  def toMap: F[Map[Seq[C], Row[P, A, K]]] =
    for {
      cache         <- S.read
      data          = cache.data.readOnlySnapshot().map(_.leftMap(Seq(_))).toMap
      continuations = (cache.continuations ++ cache.installedContinuations.mapValues(Seq(_))).toMap
      zipped        = zip(data, continuations, Seq.empty[Datum[A]], Seq.empty[WaitingContinuation[P, K]])
      mapped        = zipped.mapValues { case (d, k) => Row(d, k) }
    } yield mapped.filter { case (_, v) => !(v.data.isEmpty && v.wks.isEmpty) }
}

object HotStore {

  def inMem[F[_]: Sync, C, P, A, K](
      implicit S: Cell[F, Cache[C, P, A, K]],
      HR: HistoryReader[F, C, P, A, K],
      ck: Codec[K]
  ): HotStore[F, C, P, A, K] =
    new InMemHotStore[F, C, P, A, K]

  def from[F[_], C, P, A, K](
      cache: Cache[C, P, A, K],
      historyReader: HistoryReader[F, C, P, A, K]
  )(implicit ck: Codec[K], sync: Sync[F]) =
    for {
      cache <- Cell.refCell[F, Cache[C, P, A, K]](cache)
      store = HotStore.inMem(Sync[F], cache, historyReader, ck)
    } yield store
}
