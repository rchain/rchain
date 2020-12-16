package coop.rchain.rspace

import cats._
import cats.effect._
import cats.effect.concurrent.{Deferred, Ref}
import cats.implicits._
import coop.rchain.rspace.internal._
import coop.rchain.shared.MapOps._
import coop.rchain.shared.Serialize._
import scodec.Codec

import scala.collection.immutable.Map

/**
  * HotStore is an optimisation over HistoryRepository.
  * Instead of manipulating with data on disk, rholang execution is done in memory and once it finish,
  * result is committed by calling checkpoint.
  *
  * HotStore extends HistoryReader because when touching any channel it has to fetch cold data on this.
  * In addition to HistoryReader HotStore defines methods to mutate the state + methods to install
  * arbitrary continuations and joins. The latter is used to enable effects for onchain execution, which are
  * not neccessary to store on chain. E.g. `rho:io:stdout` process that writes to standard output
  * is not stored on chain, but instead installed in HotStore for each rholang execution.
  */
trait HotStore[F[_], C, P, A, K] extends HistoryReader[F, C, P, A, K] {
  def installContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit]
  def installJoin(channel: C, join: Seq[C]): F[Unit]

  def putContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit]
  def removeContinuation(channels: Seq[C], index: Int): F[Unit]

  def putDatum(channel: C, d: Datum[A]): F[Unit]
  def removeDatum(channel: C, index: Int): F[Unit]

  def putJoin(channel: C, join: Seq[C]): F[Unit]
  def removeJoin(channel: C, join: Seq[C]): F[Unit]

  def changes(): F[Seq[HotStoreAction]]
  def toMap: F[Map[Seq[C], Row[P, A, K]]]
  def snapshot(): F[HotStoreState[C, P, A, K]]
}

final case class HotStoreState[C, P, A, K](
    continuations: Map[Seq[C], Seq[WaitingContinuation[P, K]]] =
      Map.empty[Seq[C], Seq[WaitingContinuation[P, K]]],
    installedContinuations: Map[Seq[C], WaitingContinuation[P, K]] =
      Map.empty[Seq[C], WaitingContinuation[P, K]],
    data: Map[C, Seq[Datum[A]]] = Map.empty[C, Seq[Datum[A]]],
    joins: Map[C, Seq[Seq[C]]] = Map.empty[C, Seq[Seq[C]]],
    installedJoins: Map[C, Seq[Seq[C]]] = Map.empty[C, Seq[Seq[C]]]
)

private class InMemHotStore[F[_]: Concurrent, C, P, A, K](
    contRef: Ref[F, Map[Seq[C], Seq[WaitingContinuation[P, K]]]],
    instContRef: Ref[F, Map[Seq[C], WaitingContinuation[P, K]]],
    dataRef: Ref[F, Map[C, Seq[Datum[A]]]],
    joinsRef: Ref[F, Map[C, Seq[Seq[C]]]],
    instJoinsRef: Ref[F, Map[C, Seq[Seq[C]]]],
    // this is what is inside history store, lazily populated. Starting data for HotStoreState
    coldStoreConts: Ref[F, Map[Seq[C], Deferred[F, Seq[WaitingContinuation[P, K]]]]],
    coldStoreData: Ref[F, Map[C, Deferred[F, Seq[Datum[A]]]]],
    coldStoreJoins: Ref[F, Map[C, Deferred[F, Seq[Seq[C]]]]]
)(
    implicit coldDataReader: HistoryReader[F, C, P, A, K],
    ck: Codec[K]
) extends HotStore[F, C, P, A, K] {

  def snapshot(): F[HotStoreState[C, P, A, K]] =
    for {
      c  <- contRef.get
      ic <- instContRef.get
      d  <- dataRef.get
      j  <- joinsRef.get
      ij <- instJoinsRef.get
    } yield HotStoreState(c, ic, d, j, ij)

  implicit val codec = fromCodec(ck)

  // Continuations

  def getContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]] =
    for {
      _                      <- initHotContinuations(channels)
      continuations          <- contRef.get
      installedContinuations <- instContRef.get
      r                      = installedContinuations.get(channels) ++: continuations(channels)
    } yield r

  def putContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit] =
    for {
      _ <- initHotContinuations(channels)
      _ <- contRef.update { state =>
            val curVal = state(channels)
            val newVal = wc +: curVal
            state.updated(channels, newVal)
          }
    } yield ()

  def installContinuation(channels: Seq[C], wc: WaitingContinuation[P, K]): F[Unit] =
    instContRef.update { v =>
      v.updated(channels, wc)
    }

  def removeContinuation(channels: Seq[C], index: Int): F[Unit] =
    for {
      _         <- initHotContinuations(channels)
      instConts <- instContRef.get
      r <- contRef.modify[(Boolean, Boolean)](state => {
            val curVal      = state(channels)
            val isInstalled = instConts.contains(channels)

            val removingInstalled = (isInstalled && index == 0)
            val outOfBounds       = !curVal.isDefinedAt(index)

            if (removingInstalled || outOfBounds)
              (state, (removingInstalled, outOfBounds))
            else {
              val newVal = removeIndex(curVal, index)
              // TODO Logic behind this isInstalled check is unclear
              if (isInstalled)
                (
                  state.updated(channels, newVal.tail),
                  (false, false)
                )
              else
                (
                  state.updated(channels, newVal),
                  (false, false)
                )
            }
          })
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
      _      <- initHotData(channel)
      datums <- dataRef.get
      r      = datums(channel)
    } yield r

  def putDatum(channel: C, datum: Datum[A]): F[Unit] =
    for {
      _ <- initHotData(channel)
      _ <- dataRef.update { state =>
            val curVal = state(channel)
            val newVal = datum +: curVal
            state.updated(channel, newVal)
          }
    } yield ()

  def removeDatum(channel: C, index: Int): F[Unit] =
    for {
      _ <- initHotData(channel)
      err <- dataRef.modify[Boolean](state => {
              val curVal      = state(channel)
              val outOfBounds = !curVal.isDefinedAt(index)

              if (outOfBounds)
                (state, outOfBounds)
              else {
                val newVal = removeIndex(curVal, index)
                (state.updated(channel, newVal), false)
              }
            })
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
      _              <- initHotJoins(channel)
      joins          <- joinsRef.get
      installedJoins <- instJoinsRef.get
      r              = installedJoins.getOrElse(channel, Seq.empty[Seq[C]]) ++: joins(channel)
    } yield r

  def putJoin(channel: C, join: Seq[C]): F[Unit] =
    for {
      _ <- initHotJoins(channel)
      _ <- joinsRef.update { state =>
            val curVal = state(channel)
            if (curVal.contains(join))
              state
            else {
              val newVal = join +: curVal
              state.updated(channel, newVal)
            }
          }
    } yield ()

  def installJoin(channel: C, join: Seq[C]): F[Unit] = instJoinsRef.update { state =>
    val curInstalled = state.getOrElse(channel, Seq.empty)
    if (!curInstalled.contains(join)) {
      val newVal = join +: curInstalled
      state.updated(channel, newVal)
    } else
      state
  }

  def removeJoin(channel: C, join: Seq[C]): F[Unit] =
    for {
      _              <- initHotJoins(channel)
      _              <- initHotContinuations(join)
      contsAtJoin    <- contRef.get.map(_.getOrElse(join, Seq.empty[WaitingContinuation[P, K]]))
      instContAtJoin <- instContRef.get.map(_.get(join))
      conts          = contsAtJoin ++ instContAtJoin
      err <- joinsRef.modify[Boolean](state => {
              val curJoins = state(channel)

              val index       = state(channel).indexOf(join)
              val outOfBounds = !curJoins.isDefinedAt(index)

              // TODO should attimpting to remove join with non empty contnuation lead to error as well?
              val doRemove = conts.isEmpty && index != -1

              if (doRemove) {
                if (outOfBounds)
                  (state, true)
                else {
                  val newVal = removeIndex(state(channel), index)
                  (state.updated(channel, newVal), false)
                }
              } else (state, false)
            })
      _ <- Sync[F]
            .raiseError(
              new IndexOutOfBoundsException(
                s"Index out of bounds when removing join"
              )
            )
            .whenA(err)

    } yield ()

  def changes(): F[Seq[HotStoreAction]] =
    for {
      conts <- contRef.get.map(_.map {
                case (k, v) if (v.isEmpty) => DeleteContinuations(k)
                case (k, v)                => InsertContinuations(k, v)
              }.toVector)
      data <- dataRef.get.map(_.map {
               case (k, v) if (v.isEmpty) => DeleteData(k)
               case (k, v)                => InsertData(k, v)
             }.toVector)
      joins <- joinsRef.get.map(_.map {
                case (k, v) if (v.isEmpty) => DeleteJoins(k)
                case (k, v)                => InsertJoins(k, v)
              }.toVector)
    } yield (conts ++ data ++ joins)

  private def removeIndex[E](col: Seq[E], index: Int): Seq[E] = {
    val (l1, l2) = col splitAt index
    (l1 ++ (l2 tail))
  }

  def toMap: F[Map[Seq[C], Row[P, A, K]]] =
    for {
      dataMap       <- dataRef.get
      contMap       <- contRef.get
      instConts     <- instContRef.get
      data          = dataMap.map(_.leftMap(Seq(_)))
      continuations = (contMap ++ instConts.mapValues(Seq(_)))
      zipped        = zip(data, continuations, Seq.empty[Datum[A]], Seq.empty[WaitingContinuation[P, K]])
      mapped        = zipped.mapValues { case (d, k) => Row(d, k) }
    } yield mapped.filter { case (_, v) => !(v.data.isEmpty && v.wks.isEmpty) }

  // When hotstore tries to operate on particular channel first time, it have to pull data from coldstore first.
  // The following 3 methods do this, Deferred ensures that there are no races and pulling done only once.
  // It is cheap to call these methods - all calls except the first one return pulled value
  // stored in resolved Deferred.

  private def initHotContinuations(channels: Seq[C]) =
    for {
      d <- Deferred[F, Seq[WaitingContinuation[P, K]]]
      // read deferred object for channels required, or store created above if not present
      r <- coldStoreConts
            .modify[(Deferred[F, Seq[WaitingContinuation[P, K]]], Boolean)](cache => {
              cache.get(channels) match {
                case Some(v) => (cache, (v, false))
                case None =>
                  (cache + (channels -> d), (d, true))
              }
            })
      // on first call for data on `channels` stored deferred should be resolved.
      (deferred, readColdData) = r
      _ <- Applicative[F].whenA(readColdData)(for {
            coldValue <- coldDataReader.getContinuations(channels)
            _ <- contRef.update(
                  s => s.updated(channels, coldValue)
                )
            _ <- deferred.complete(coldValue)
          } yield ())
      // all calls wait here once the very first one resolves Deferred object
      contInHistoryStore <- deferred.get
    } yield contInHistoryStore

  private def initHotData(channel: C) =
    for {
      d <- Deferred[F, Seq[Datum[A]]]
      r <- coldStoreData
            .modify[(Deferred[F, Seq[Datum[A]]], Boolean)](cache => {
              cache.get(channel) match {
                case Some(v) => (cache, (v, false))
                case None    => (cache + (channel -> d), (d, true))
              }
            })
      (deferred, readColdData) = r
      _ <- Applicative[F].whenA(readColdData)(for {
            coldValue <- coldDataReader.getData(channel)
            _ <- dataRef.update(
                  s => s.updated(channel, coldValue)
                )
            _ <- deferred.complete(coldValue)
          } yield ())
      dataInHistoryStore <- deferred.get
    } yield dataInHistoryStore

  private def initHotJoins(channel: C) =
    for {
      d <- Deferred[F, Seq[Seq[C]]]
      r <- coldStoreJoins
            .modify[(Deferred[F, Seq[Seq[C]]], Boolean)](cache => {
              cache.get(channel) match {
                case Some(v) => (cache, (v, false))
                case None    => (cache + (channel -> d), (d, true))
              }
            })
      (deferred, readColdData) = r
      _ <- Applicative[F].whenA(readColdData)(for {
            coldValue <- coldDataReader.getJoins(channel)
            _ <- joinsRef.update(
                  s => s.updated(channel, coldValue)
                )
            _ <- deferred.complete(coldValue)
          } yield ())
      joinsInHistoryStore <- deferred.get
    } yield joinsInHistoryStore
}

object HotStore {

  def inMem[F[_], C, P, A, K](startState: Option[HotStoreState[C, P, A, K]] = None)(
      implicit HR: HistoryReader[F, C, P, A, K],
      ck: Codec[K],
      c: Concurrent[F]
  ): F[HotStore[F, C, P, A, K]] =
    for {
      // state Refs
      contRef <- Ref.of[F, Map[Seq[C], Seq[WaitingContinuation[P, K]]]](
                  startState.map(_.continuations).getOrElse(Map.empty)
                )
      instContRef <- Ref.of[F, Map[Seq[C], WaitingContinuation[P, K]]](
                      startState.map(_.installedContinuations).getOrElse(Map.empty)
                    )
      dataRef  <- Ref.of[F, Map[C, Seq[Datum[A]]]](startState.map(_.data).getOrElse(Map.empty))
      joinsRef <- Ref.of[F, Map[C, Seq[Seq[C]]]](startState.map(_.joins).getOrElse(Map.empty))
      instJoinsRef <- Ref.of[F, Map[C, Seq[Seq[C]]]](
                       startState.map(_.installedJoins).getOrElse(Map.empty)
                     )

      // containers for deferred results from cold store
      coldConts <- Ref.of[F, Map[Seq[C], Deferred[F, Seq[WaitingContinuation[P, K]]]]](Map.empty)
      coldData  <- Ref.of[F, Map[C, Deferred[F, Seq[Datum[A]]]]](Map.empty)
      coldJoins <- Ref.of[F, Map[C, Deferred[F, Seq[Seq[C]]]]](Map.empty)
    } yield new InMemHotStore[F, C, P, A, K](
      contRef,
      instContRef,
      dataRef,
      joinsRef,
      instJoinsRef,
      coldConts,
      coldData,
      coldJoins
    )

  def from[F[_], C, P, A, K](
      cache: HotStoreState[C, P, A, K],
      historyReader: HistoryReader[F, C, P, A, K]
  )(implicit ck: Codec[K], concurrent: Concurrent[F]) =
    for {
      store <- HotStore.inMem(cache.some)(historyReader, ck, Concurrent[F])
    } yield store

  def empty[F[_], C, P, A, K](
      historyReader: HistoryReader[F, C, P, A, K]
  )(implicit ck: Codec[K], concurrent: Concurrent[F]) =
    from(HotStoreState(), historyReader)

}
