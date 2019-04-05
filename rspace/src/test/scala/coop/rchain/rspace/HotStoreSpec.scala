package coop.rchain.rspace

import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.{MVar, Ref}
import cats.effect.implicits._
import cats.mtl._
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.internal._
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.shared.Cell
import org.scalatest._
import org.scalatest.prop._

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

trait HotStoreSpec[F[_], M[_]] extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit def S: Sync[F]
  implicit def P: Parallel[F, M]

  type Channel      = String
  type Data         = Datum[String]
  type Continuation = WaitingContinuation[Pattern, StringsCaptor]

  def fixture(
      f: (
          Cell[F, Cache[String, Pattern, String, StringsCaptor]],
          History[F],
          HotStore[F, String, Pattern, String, StringsCaptor]
      ) => F[Unit]
  ): Unit

  "getContinuations when cache is empty" should "read from history and put into the cache" in forAll {
    (channels: Vector[Channel], historyContinuations: Vector[Continuation]) =>
      fixture { (state, history, hotStore) =>
        for {
          _                 <- history.putContinuations(channels, historyContinuations)
          cache             <- state.read
          _                 <- S.delay(cache.continuations shouldBe empty)
          readContinuations <- hotStore.getContinuations(channels)
          cache             <- state.read
          _                 <- S.delay(cache.continuations(channels) shouldEqual historyContinuations)
          _                 <- S.delay(readContinuations shouldEqual historyContinuations)
        } yield ()
      }
  }

  "getContinuations when cache contains data" should "read from cache ignoring history" in forAll {
    (
        channels: Vector[Channel],
        historyContinuations: Vector[Continuation],
        cachedContinuations: Vector[Continuation]
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- history.putContinuations(channels, historyContinuations)
            _ <- state.modify(
                  _ =>
                    Cache(
                      continuations = TrieMap(
                        channels -> cachedContinuations
                      )
                    )
                )
            readContinuations <- hotStore.getContinuations(channels)

            cache <- state.read
            _     <- S.delay(cache.continuations(channels) shouldEqual cachedContinuations)
            _     <- S.delay(readContinuations shouldEqual cachedContinuations)
          } yield ()
        }
      }
  }

  "putContinuation when cache is empty" should "read from history and add to it" in forAll {
    (
        channels: Vector[Channel],
        historyContinuations: Vector[Continuation],
        insertedContinuation: Continuation
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- history.putContinuations(channels, historyContinuations)
            _ <- hotStore.putContinuation(channels, insertedContinuation)

            cache <- state.read
            _ <- S.delay(
                  cache.continuations(channels) shouldEqual insertedContinuation +: historyContinuations
                )
          } yield ()
        }
      }
  }

  "putContinuation when cache contains data" should "read from the cache and add to it" in forAll {
    (
        channels: Vector[Channel],
        historyContinuations: Vector[Continuation],
        cachedContinuations: Vector[Continuation],
        insertedContinuation: Continuation
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- history.putContinuations(channels, historyContinuations)
            _ <- state.modify(
                  _ =>
                    Cache(
                      continuations = TrieMap(
                        channels -> cachedContinuations
                      )
                    )
                )
            _     <- hotStore.putContinuation(channels, insertedContinuation)
            cache <- state.read
            _ <- S.delay(
                  cache
                    .continuations(channels) shouldEqual insertedContinuation +: cachedContinuations
                )
          } yield ()
        }
      }
  }

  "getData when cache is empty" should "read from history and put into the cache" in forAll {
    (channel: Channel, historyData: Vector[Datum[String]]) =>
      fixture { (state, history, hotStore) =>
        for {
          _        <- history.putData(channel, historyData)
          cache    <- state.read
          _        = cache.data shouldBe empty
          readData <- hotStore.getData(channel)
          cache    <- state.read
          _        <- S.delay(cache.data(channel) shouldEqual historyData)
          _        <- S.delay(readData shouldEqual historyData)
        } yield ()
      }
  }

  "getData when cache contains data" should "read from cache ignoring history" in forAll {
    (
        channel: Channel,
        historyData: Vector[Data],
        cachedData: Vector[Data]
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- history.putData(channel, historyData)
            _ <- state.modify(
                  _ =>
                    Cache(
                      data = TrieMap(
                        channel -> cachedData
                      )
                    )
                )
            readData <- hotStore.getData(channel)
            cache    <- state.read
            _        <- S.delay(cache.data(channel) shouldEqual cachedData)
            _        <- S.delay(readData shouldEqual cachedData)
          } yield ()
        }
      }
  }

  "putData when cache is empty" should "read from history and add to it" in forAll {
    (
        channel: Channel,
        historyData: Vector[Data],
        insertedData: Data
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _     <- history.putData(channel, historyData)
            _     <- hotStore.putDatum(channel, insertedData)
            cache <- state.read
            _     <- S.delay(cache.data(channel) shouldEqual insertedData +: historyData)
          } yield ()
        }
      }
  }

  "putData when cache contains data" should "read from the cache and add to it" in forAll {
    (
        channel: Channel,
        historyData: Vector[Data],
        cachedData: Vector[Data],
        insertedData: Data
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- history.putData(channel, historyData)
            _ <- state.modify(
                  _ =>
                    Cache(
                      data = TrieMap(
                        channel -> cachedData
                      )
                    )
                )
            _     <- hotStore.putDatum(channel, insertedData)
            cache <- state.read
            _     <- S.delay(cache.data(channel) shouldEqual insertedData +: cachedData)
          } yield ()
        }
      }
  }

  "getJoins when cache is empty" should "read from history and put into the cache" in forAll {
    (channel: Channel, historyJoins: Vector[Vector[Channel]]) =>
      fixture { (state, history, hotStore) =>
        for {
          _         <- history.putJoins(channel, historyJoins)
          cache     <- state.read
          _         = cache.joins shouldBe empty
          readJoins <- hotStore.getJoins(channel)
          cache     <- state.read
          _         = cache.joins(channel) shouldEqual historyJoins
          _         <- S.delay(readJoins shouldEqual historyJoins)
        } yield ()
      }
  }

  "getJoins when cache contains data" should "read from cache ignoring history" in forAll {
    (
        channel: Channel,
        historyJoins: Vector[Vector[Channel]],
        cachedJoins: Vector[Vector[Channel]]
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- history.putJoins(channel, historyJoins)
            _ <- state.modify(
                  _ =>
                    Cache(
                      joins = TrieMap(
                        channel -> cachedJoins
                      )
                    )
                )
            readJoins <- hotStore.getJoins(channel)
            cache     <- state.read
            _         <- S.delay(cache.joins(channel) shouldEqual cachedJoins)
            _         <- S.delay(readJoins shouldEqual cachedJoins)
          } yield ()
        }
      }
  }

  "concurrent data operations on disjoint channels" should "not mess up the cache" in forAll {
    (
        channel1: Channel,
        channel2: Channel,
        historyData1: Vector[Data],
        historyData2: Vector[Data],
        insertedData1: Data,
        insertedData2: Data
    ) =>
      whenever(channel1 =!= channel2) {
        fixture { (state, history, hotStore) =>
          {
            for {
              _ <- history.putData(channel1, historyData1)
              _ <- history.putData(channel2, historyData2)
              _ <- Vector(
                    hotStore.putDatum(channel1, insertedData1),
                    hotStore.putDatum(channel2, insertedData2)
                  ).parSequence
              r1 <- hotStore.getData(channel1)
              r2 <- hotStore.getData(channel2)
              _  <- S.delay(r1 shouldEqual insertedData1 +: historyData1)
              _  <- S.delay(r2 shouldEqual insertedData2 +: historyData2)
            } yield ()
          }
        }
      }
  }

  "concurrent continuation operations on disjoint channels" should "not mess up the cache" in forAll {
    (
        channels1: Vector[Channel],
        channels2: Vector[Channel],
        historyContinuations1: Vector[Continuation],
        historyContinuations2: Vector[Continuation],
        insertedContinuation1: Continuation,
        insertedContinuation2: Continuation
    ) =>
      whenever(channels1 =!= channels2) {
        fixture { (state, history, hotStore) =>
          {
            for {
              _ <- history.putContinuations(channels1, historyContinuations1)
              _ <- history.putContinuations(channels2, historyContinuations2)
              _ <- Vector(
                    hotStore.putContinuation(channels1, insertedContinuation1),
                    hotStore.putContinuation(channels2, insertedContinuation2)
                  ).parSequence
              r1 <- hotStore.getContinuations(channels1)
              r2 <- hotStore.getContinuations(channels2)
              _  <- S.delay(r1 shouldEqual insertedContinuation1 +: historyContinuations1)
              _  <- S.delay(r2 shouldEqual insertedContinuation2 +: historyContinuations2)
            } yield ()
          }
        }
      }
  }
}

class History[F[_]: Sync](implicit R: Cell[F, Cache[String, Pattern, String, StringsCaptor]])
    extends HistoryReader[F, String, Pattern, String, StringsCaptor] {

  def getJoins(channel: String): F[Seq[Seq[String]]] = R.read.map(_.joins(channel))
  def putJoins(channel: String, joins: Seq[Seq[String]]): F[Unit] = R.flatModify { prev =>
    Sync[F].delay(prev.joins.put(channel, joins)).map(_ => prev)
  }

  def getData(channel: String): F[Seq[Datum[String]]] = R.read.map(_.data(channel))
  def putData(channel: String, data: Seq[Datum[String]]): F[Unit] = R.flatModify { prev =>
    Sync[F].delay(prev.data.put(channel, data)).map(_ => prev)
  }

  def getContinuations(
      channels: Seq[String]
  ): F[Seq[WaitingContinuation[Pattern, StringsCaptor]]] = R.read.map(_.continuations(channels))
  def putContinuations(
      channels: Seq[String],
      continuations: Seq[WaitingContinuation[Pattern, StringsCaptor]]
  ): F[Unit] = R.flatModify { prev =>
    Sync[F].delay(prev.continuations.put(channels, continuations)).map(_ => prev)
  }
}

trait InMemHotStoreSpec extends HotStoreSpec[Task, Task.Par] {

  protected type F[A] = Task[A]
  override implicit val S: Sync[F]                  = implicitly[Concurrent[Task]]
  override implicit val P: Parallel[Task, Task.Par] = Task.catsParallel
  def C: F[Cell[F, Cache[String, Pattern, String, StringsCaptor]]]

  override def fixture(
      f: (
          Cell[F, Cache[String, Pattern, String, StringsCaptor]],
          History[F],
          HotStore[F, String, Pattern, String, StringsCaptor]
      ) => F[Unit]
  ) =
    (for {
      historyState <- Cell.refCell[F, Cache[String, Pattern, String, StringsCaptor]](
                       Cache[String, Pattern, String, StringsCaptor]()
                     )
      history = {
        implicit val hs = historyState
        new History[F]
      }
      cache <- C
      hotStore = {
        implicit val hr = history
        implicit val c  = cache
        HotStore.inMem[Task, String, Pattern, String, StringsCaptor]
      }
      res <- f(cache, history, hotStore)
    } yield res).runSyncUnsafe(1.second)

}

class MVarCachedInMemHotStoreSpec extends InMemHotStoreSpec {
  override implicit def C: F[Cell[F, Cache[String, Pattern, String, StringsCaptor]]] =
    Cell.mvarCell[F, Cache[String, Pattern, String, StringsCaptor]](Cache())
}

class RefCachedInMemHotStoreSpec extends InMemHotStoreSpec {
  override implicit def C: F[Cell[F, Cache[String, Pattern, String, StringsCaptor]]] =
    Cell.refCell[F, Cache[String, Pattern, String, StringsCaptor]](Cache())

}
