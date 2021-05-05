package coop.rchain.rspace

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.rspace.history.HistoryReaderBase
import coop.rchain.rspace.internal._
import coop.rchain.shared.Language._
import coop.rchain.shared.MapOps._

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

private class InMemHotStore[F[_]: Concurrent, C, P, A, K](
    cacheRef: Ref[F, Cache[C, P, A, K]],
    historyReader: HistoryReaderBase[F, C, P, A, K]
) extends HotStore[F, C, P, A, K] {

  def snapshot(): F[Snapshot[C, P, A, K]] = cacheRef.get.map(_.snapshot())

  def getContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]] =
    for {
      cached <- internalGetContinuations(channels)
      state  <- cacheRef.get
      result = state.installedContinuations.get(channels) ++: cached
    } yield result

  private[this] def internalGetContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]] =
    for {
      state <- cacheRef.get
      res <- state.continuations.get(channels) match {
              case None =>
                for {
                  historyContinuations <- historyReader.getContinuations(channels)
                  _ <- cacheRef.update { cache =>
                        ignore(cache.continuations.putIfAbsent(channels, historyContinuations))
                        cache
                      }
                } yield historyContinuations
              case Some(continuations) => continuations.pure[F]
            }
    } yield (res)

  def putContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit] =
    for {
      continuations <- internalGetContinuations(channels)
      _ <- cacheRef.update { cache =>
            ignore(cache.continuations.put(channels, wc +: continuations))
            cache
          }
    } yield ()

  def installContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit] =
    cacheRef.update { cache =>
      ignore(cache.installedContinuations.put(channels, wc))
      cache
    }

  def removeContinuation(channels: Seq[C], index: Int): F[Unit] =
    for {
      continuations <- getContinuations(channels)
      cache         <- cacheRef.get
      installed     = cache.installedContinuations.get(channels)
      _ <- if (installed.isDefined && index == 0) {
            new IllegalArgumentException("Attempted to remove an installed continuation").raiseError
          } else
            checkIndex(continuations, index) >> cacheRef.update { cache =>
              val updated = removeIndex(
                continuations,
                index
              )
              ignore(installed match {
                case Some(_) => cache.continuations.put(channels, updated tail)
                case None    => cache.continuations.put(channels, updated)
              })
              cache
            }
    } yield ()

  def getData(channel: C): F[Seq[Datum[A]]] =
    for {
      state <- cacheRef.get
      res <- state.data.get(channel) match {
              case None =>
                for {
                  historyData <- historyReader.getData(channel)
                  _ <- cacheRef.update { cache =>
                        ignore(cache.data.putIfAbsent(channel, historyData))
                        cache
                      }
                } yield (historyData)
              case Some(data) => data.pure[F]
            }
    } yield (res)

  def putDatum(channel: C, datum: Datum[A]): F[Unit] =
    for {
      data <- getData(channel)
      _ <- cacheRef.update { cache =>
            ignore(cache.data.put(channel, datum +: data))
            cache
          }
    } yield ()

  def removeDatum(channel: C, index: Int): F[Unit] =
    for {
      data <- getData(channel)
      s    <- cacheRef.get
      _    <- checkIndex(s.data(channel), index)
      _ <- cacheRef.update { cache =>
            val updated = removeIndex(cache.data(channel), index)
            ignore(cache.data.put(channel, updated))
            cache

          }
    } yield ()

  def getJoins(channel: C): F[Seq[Seq[C]]] =
    for {
      cached <- internalGetJoins(channel)
      state  <- cacheRef.get
    } yield state.installedJoins.getOrElse(channel, Seq.empty) ++: cached

  private[this] def internalGetJoins(channel: C): F[Seq[Seq[C]]] =
    for {
      state <- cacheRef.get
      res <- state.joins.get(channel) match {
              case None =>
                for {
                  historyJoins <- historyReader.getJoins(channel)
                  _ <- cacheRef.update { cache =>
                        ignore(cache.joins.putIfAbsent(channel, historyJoins))
                        cache
                      }
                } yield (historyJoins)
              case Some(joins) => joins.pure[F]
            }
    } yield res

  def putJoin(channel: C, join: Seq[C]): F[Unit] =
    for {
      joins <- getJoins(channel)
      _ <- cacheRef
            .update { cache =>
              ignore(cache.joins.put(channel, join +: joins))
              cache
            }
            .whenA(!joins.contains(join))
    } yield ()

  def installJoin(channel: C, join: Seq[C]): F[Unit] = cacheRef.update { cache =>
    ignore {
      val installed = cache.installedJoins.getOrElse(channel, Seq.empty)

      if (!installed.contains(join))
        cache.installedJoins
          .put(channel, join +: installed)
    }
    cache
  }

  def removeJoin(channel: C, join: Seq[C]): F[Unit] =
    for {
      joins         <- internalGetJoins(channel)
      continuations <- getContinuations(join)
      s             <- cacheRef.get
      index         = s.joins(channel).indexOf(join)
      _ <- if (continuations.isEmpty && index != -1)
            checkIndex(s.joins(channel), index) >>
              cacheRef.update { cache =>
                val updated =
                  removeIndex(
                    cache.joins(
                      channel
                    ),
                    index
                  )
                ignore(
                  cache.joins.put(
                    channel,
                    updated
                  )
                )
                cache
              } else ().pure[F]
    } yield ()

  def changes(): F[Seq[HotStoreAction]] =
    for {
      cache <- cacheRef.get
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
    } yield continuations ++ data ++ joins

  private def checkIndex[E](col: Seq[E], index: Int): F[Unit] =
    new IndexOutOfBoundsException(
      s"Tried to remove index ${index} from a Vector of size ${col.size}"
    ).raiseError.unlessA(col.isDefinedAt(index))

  def toMap: F[Map[Seq[C], Row[P, A, K]]] =
    for {
      cache         <- cacheRef.get
      data          = cache.data.readOnlySnapshot().map(_.leftMap(Seq(_))).toMap
      continuations = (cache.continuations ++ cache.installedContinuations.mapValues(Seq(_))).toMap
      zipped        = zip(data, continuations, Seq.empty[Datum[A]], Seq.empty[WaitingContinuation[P, K]])
      mapped        = zipped.mapValues { case (d, k) => Row(d, k) }
    } yield mapped.filter { case (_, v) => !(v.data.isEmpty && v.wks.isEmpty) }
}

object HotStore {

  def inMem[F[_]: Concurrent, C, P, A, K](
      cache: Ref[F, Cache[C, P, A, K]],
      historyReader: HistoryReaderBase[F, C, P, A, K]
  ): HotStore[F, C, P, A, K] =
    new InMemHotStore[F, C, P, A, K](cache, historyReader)

  def from[F[_]: Concurrent, C, P, A, K](
      cache: Cache[C, P, A, K],
      historyReader: HistoryReaderBase[F, C, P, A, K]
  ): F[HotStore[F, C, P, A, K]] =
    for {
      cache <- Ref.of[F, Cache[C, P, A, K]](cache)
      store = HotStore.inMem(cache, historyReader)
    } yield store

  def empty[F[_]: Concurrent, C, P, A, K](
      historyReader: HistoryReaderBase[F, C, P, A, K]
  ): F[HotStore[F, C, P, A, K]] = from(Cache(), historyReader)

}
