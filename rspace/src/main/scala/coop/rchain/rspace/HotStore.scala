package coop.rchain.rspace

import cats.syntax.all._
import cats.effect._
import cats.effect.concurrent.{Deferred, Ref}
import coop.rchain.rspace.history.HistoryReaderBase
import coop.rchain.shared.MapOps._
import coop.rchain.rspace.internal._

import scala.collection.immutable.Map

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
  def snapshot(): F[HotStoreState[C, P, A, K]]
}

final case class HotStoreState[C, P, A, K](
    continuations: Map[Seq[C], Seq[WaitingContinuation[P, K]]],
    installedContinuations: Map[Seq[C], WaitingContinuation[P, K]],
    data: Map[C, Seq[Datum[A]]],
    joins: Map[C, Seq[Seq[C]]],
    installedJoins: Map[C, Seq[Seq[C]]]
)

object HotStoreState {
  def apply[C, P, A, K](
      continuations: Map[Seq[C], Seq[WaitingContinuation[P, K]]] =
        Map.empty[Seq[C], Seq[WaitingContinuation[P, K]]],
      installedContinuations: Map[Seq[C], WaitingContinuation[P, K]] =
        Map.empty[Seq[C], WaitingContinuation[P, K]],
      data: Map[C, Seq[Datum[A]]] = Map.empty[C, Seq[Datum[A]]],
      joins: Map[C, Seq[Seq[C]]] = Map.empty[C, Seq[Seq[C]]],
      installedJoins: Map[C, Seq[Seq[C]]] = Map.empty[C, Seq[Seq[C]]]
  ): HotStoreState[C, P, A, K] =
    HotStoreState[C, P, A, K](continuations, installedContinuations, data, joins, installedJoins)
}

private final case class HistoryStoreCache[F[_], C, P, A, K](
    continuations: Map[Seq[C], Deferred[F, Seq[WaitingContinuation[P, K]]]],
    datums: Map[C, Deferred[F, Seq[Datum[A]]]],
    joins: Map[C, Deferred[F, Seq[Seq[C]]]]
)

private class InMemHotStore[F[_]: Concurrent, C, P, A, K](
    hotStoreState: Ref[F, HotStoreState[C, P, A, K]],
    // this is what is inside history store, lazily populated. Starting data for HotStoreState
    historyStoreCache: Ref[F, HistoryStoreCache[F, C, P, A, K]],
    historyReaderBase: HistoryReaderBase[F, C, P, A, K]
) extends HotStore[F, C, P, A, K] {

  def snapshot(): F[HotStoreState[C, P, A, K]] = hotStoreState.get

  // Continuations

  def getContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]] =
    for {
      fromHistoryStore <- getContFromHistoryStore(channels)
      result <- hotStoreState.modify[Seq[WaitingContinuation[P, K]]] { state =>
                 state.continuations.get(channels) match {
                   // update with what is in historyStore and return the same
                   case None =>
                     (
                       state.copy(
                         continuations = state.continuations.updated(channels, fromHistoryStore)
                       ),
                       state.installedContinuations.get(channels) ++: fromHistoryStore
                     )
                   // just return what is in hot store already
                   case Some(continuations) =>
                     (state, state.installedContinuations.get(channels) ++: continuations)
                 }
               }
    } yield result

  def putContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit] =
    for {
      fromHistoryStore <- getContFromHistoryStore(channels)
      _ <- hotStoreState.update { state =>
            val curVal = state.continuations.getOrElse(channels, fromHistoryStore)
            val newVal = wc +: curVal
            state.copy(continuations = state.continuations.updated(channels, newVal))
          }
    } yield ()

  def installContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit] =
    hotStoreState.update { cache =>
      cache.copy(installedContinuations = cache.installedContinuations.updated(channels, wc))
    }

  def removeContinuation(channels: Seq[C], index: Int): F[Unit] =
    for {
      fromHistoryStore <- getContFromHistoryStore(channels)
      r <- hotStoreState.modify[(Boolean, Boolean)] { state =>
            val curVal       = state.continuations.getOrElse(channels, fromHistoryStore)
            val installedCon = state.installedContinuations.get(channels)
            val isInstalled  = installedCon.nonEmpty

            val removingInstalled = (isInstalled && index == 0)
            // why -1 here
            // from users who use `HotStore` trait perspective
            // the users need to call getContinuations first to retrieve the continuation of the channels
            // then find which index of the continuation users want to remove
            // while getContinuations would return results like `Vector(installedContinuation, normalcontinuations, ...)`
            // The first one would be always `installedContinuation` if it exist which need the
            // index to -1 to exclude installedContinuation
            val removedIndex = if (isInstalled) index - 1 else index
            val outOfBounds  = !curVal.isDefinedAt(removedIndex)

            if (removingInstalled || outOfBounds)
              (
                state.copy(continuations = state.continuations.updated(channels, curVal)),
                (removingInstalled, outOfBounds)
              )
            else {
              val newVal = removeIndex(curVal, removedIndex)
              (
                state.copy(continuations = state.continuations.updated(channels, newVal)),
                (false, false)
              )
            }
          }
      (removingInstalled, invalidIndex) = r
      _ <- Sync[F]
            .raiseError {
              new IllegalArgumentException("Attempted to remove an installed continuation")
            }
            .whenA(removingInstalled)
      _ <- Sync[F]
            .raiseError(
              new IndexOutOfBoundsException(
                s"Index ${index} out of bounds when removing continuation"
              )
            )
            .whenA(invalidIndex)
    } yield ()

  // Data

  def getData(channel: C): F[Seq[Datum[A]]] =
    for {
      fromHistoryStore <- getDataFromHistoryStore(channel)
      result <- hotStoreState.modify[Seq[Datum[A]]] { state =>
                 state.data.get(channel) match {
                   // update with what is in historyStore and return the same
                   case None =>
                     (
                       state.copy(
                         data = state.data.updated(channel, fromHistoryStore)
                       ),
                       fromHistoryStore
                     )
                   // just return what is in hot store already
                   case Some(data) => (state, data)
                 }
               }
    } yield (result)

  def putDatum(channel: C, datum: Datum[A]): F[Unit] =
    for {
      fromHistoryStore <- getDataFromHistoryStore(channel)
      _ <- hotStoreState.update { state =>
            state.copy(
              data = state.data
              // if continuation list is empty, add what there is in history store as well
                .updated(
                  channel,
                  datum +: state.data.getOrElse(channel, fromHistoryStore)
                )
            )
          }
    } yield ()

  def removeDatum(channel: C, index: Int): F[Unit] =
    for {
      fromHistoryStore <- getDataFromHistoryStore(channel)
      err <- hotStoreState.modify[Boolean] { state =>
              val curVal      = state.data.getOrElse(channel, fromHistoryStore)
              val outOfBounds = !curVal.isDefinedAt(index)

              if (outOfBounds)
                (state.copy(data = state.data.updated(channel, curVal)), outOfBounds)
              else {
                val updated = removeIndex(curVal, index)
                (state.copy(data = state.data.updated(channel, updated)), false)
              }
            }
      _ <- Sync[F]
            .raiseError(
              new IndexOutOfBoundsException(
                s"Index ${index} out of bounds when removing datum"
              )
            )
            .whenA(err)
    } yield ()

  // Joins

  def getJoins(channel: C): F[Seq[Seq[C]]] =
    for {
      fromHistoryStore <- getJoinsFromHistoryStore(channel)
      result <- hotStoreState.modify[Seq[Seq[C]]] { state =>
                 state.joins.get(channel) match {
                   // update with what is in historyStore and return the same
                   case None =>
                     (
                       state.copy(
                         joins = state.joins.updated(channel, fromHistoryStore)
                       ),
                       state.installedJoins.getOrElse(channel, Seq.empty) ++: fromHistoryStore
                     )
                   // just return what is in hot store already
                   case Some(joins) =>
                     (state, state.installedJoins.getOrElse(channel, Seq.empty) ++: joins)
                 }
               }

    } yield result

  def putJoin(channel: C, join: Seq[C]): F[Unit] =
    for {
      fromHistoryStore <- getJoinsFromHistoryStore(channel)
      _ <- hotStoreState.update { state =>
            val curJoins = state.joins.getOrElse(channel, fromHistoryStore)
            if (curJoins.contains(join))
              state
            else {
              val newVal = join +: curJoins
              state.copy(joins = state.joins.updated(channel, newVal))
            }
          }
    } yield ()

  def installJoin(channel: C, join: Seq[C]): F[Unit] = hotStoreState.update { state =>
    val curInstalled = state.installedJoins.getOrElse(channel, Seq.empty)
    if (!curInstalled.contains(join)) {
      val newVal = join +: curInstalled
      state.copy(installedJoins = state.installedJoins.updated(channel, newVal))
    } else
      state
  }

  def removeJoin(channel: C, join: Seq[C]): F[Unit] =
    for {
      joinsInHistoryStore <- getJoinsFromHistoryStore(channel)
      contsInHistoryStore <- getContFromHistoryStore(join)
      err <- hotStoreState.modify[Boolean] { state =>
              val curJoins = state.joins.getOrElse(channel, joinsInHistoryStore)

              val curConts = state.installedContinuations
                .get(join) ++: state.continuations.getOrElse(join, contsInHistoryStore)

              val index       = curJoins.indexOf(join)
              val outOfBounds = !curJoins.isDefinedAt(index)

              // Remove join is called when continuation is removed, so it can be called when
              // continuations are present in which case we just want to skip removal.
              val doRemove = curConts.isEmpty

              if (doRemove) {
                if (outOfBounds)
                  (state.copy(joins = state.joins.updated(channel, curJoins)), true)
                else {
                  val newVal = removeIndex(curJoins, index)
                  (state.copy(joins = state.joins.updated(channel, newVal)), false)
                }
              } else (state.copy(joins = state.joins.updated(channel, curJoins)), false)
            }
    } yield ()

  def changes(): F[Seq[HotStoreAction]] =
    for {
      cache <- hotStoreState.get
      continuations = (cache.continuations.map {
        case (k, v) if (v.isEmpty) => DeleteContinuations(k)
        case (k, v)                => InsertContinuations(k, v)
      }).toVector
      data = (cache.data.map {
        case (k, v) if (v.isEmpty) => DeleteData(k)
        case (k, v)                => InsertData(k, v)
      }).toVector
      joins = (cache.joins.map {
        case (k, v) if (v.isEmpty) => DeleteJoins(k)
        case (k, v)                => InsertJoins(k, v)
      }).toVector
    } yield (continuations ++ data ++ joins)

  private def removeIndex[E](col: Seq[E], index: Int): Seq[E] = {
    val (l1, l2) = col splitAt index
    (l1 ++ (l2 tail))
  }

  def toMap: F[Map[Seq[C], Row[P, A, K]]] =
    for {
      cache         <- hotStoreState.get
      data          = cache.data.map(_.leftMap(Seq(_))).toMap
      continuations = (cache.continuations ++ cache.installedContinuations.mapValues(Seq(_))).toMap
      zipped        = zip(data, continuations, Seq.empty[Datum[A]], Seq.empty[WaitingContinuation[P, K]])
      mapped        = zipped.mapValues { case (d, k) => Row(d, k) }
    } yield mapped.filter { case (_, v) => !(v.data.isEmpty && v.wks.isEmpty) }

  private def getContFromHistoryStore(channels: Seq[C]) =
    for {
      d <- Deferred[F, Seq[WaitingContinuation[P, K]]]
      r <- historyStoreCache
            .modify[(Deferred[F, Seq[WaitingContinuation[P, K]]], Boolean)] { cache =>
              cache.continuations.get(channels) match {
                case Some(v) => (cache, (v, true))
                case None =>
                  (cache.copy(continuations = cache.continuations + (channels -> d)), (d, false))
              }
            }
      (deferred, isComplete) = r
      _                      <- (historyReaderBase.getContinuations(channels) >>= deferred.complete).whenA(!isComplete)
      contInHistoryStore     <- deferred.get
    } yield contInHistoryStore

  private def getDataFromHistoryStore(channel: C) =
    for {
      d <- Deferred[F, Seq[Datum[A]]]
      r <- historyStoreCache
            .modify[(Deferred[F, Seq[Datum[A]]], Boolean)] { cache =>
              cache.datums.get(channel) match {
                case Some(v) => (cache, (v, true))
                case None    => (cache.copy(datums = cache.datums + (channel -> d)), (d, false))
              }
            }
      (deferred, isComplete) = r
      _                      <- (historyReaderBase.getData(channel) >>= deferred.complete).whenA(!isComplete)
      dataInHistoryStore     <- deferred.get
    } yield dataInHistoryStore

  private def getJoinsFromHistoryStore(channel: C) =
    for {
      d <- Deferred[F, Seq[Seq[C]]]
      r <- historyStoreCache
            .modify[(Deferred[F, Seq[Seq[C]]], Boolean)] { cache =>
              cache.joins.get(channel) match {
                case Some(v) => (cache, (v, true))
                case None    => (cache.copy(joins = cache.joins + (channel -> d)), (d, false))
              }
            }
      (deferred, isComplete) = r
      _                      <- (historyReaderBase.getJoins(channel) >>= deferred.complete).whenA(!isComplete)
      joinsInHistoryStore    <- deferred.get
    } yield joinsInHistoryStore
}

object HotStore {

  def apply[F[_]: Concurrent, C, P, A, K](
      hotStoreStateRef: Ref[F, HotStoreState[C, P, A, K]],
      historyReaderBase: HistoryReaderBase[F, C, P, A, K]
  ): F[HotStore[F, C, P, A, K]] =
    Ref
      .of[F, HistoryStoreCache[F, C, P, A, K]](
        HistoryStoreCache(Map.empty, Map.empty, Map.empty)
      )
      .map(new InMemHotStore[F, C, P, A, K](hotStoreStateRef, _, historyReaderBase))

  def apply[F[_]: Concurrent, C, P, A, K](
      cache: HotStoreState[C, P, A, K],
      historyReader: HistoryReaderBase[F, C, P, A, K]
  ): F[HotStore[F, C, P, A, K]] =
    for {
      cache <- Ref.of[F, HotStoreState[C, P, A, K]](cache)
      store <- HotStore(cache, historyReader)
    } yield store

  def apply[F[_]: Concurrent, C, P, A, K](
      historyReader: HistoryReaderBase[F, C, P, A, K]
  ): F[HotStore[F, C, P, A, K]] =
    apply(HotStoreState[C, P, A, K](), historyReader)

}
