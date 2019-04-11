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
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.prop._

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.util.Random
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

trait HotStoreSpec[F[_], M[_]] extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 0, minSuccessful = 20)

  implicit def S: Sync[F]
  implicit def P: Parallel[F, M]

  type Channel      = String
  type Data         = Datum[String]
  type Continuation = WaitingContinuation[Pattern, StringsCaptor]
  type Join         = Vector[Channel]
  type Joins        = Vector[Join]

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

  "getContinuations" should "include installed continuations" in forAll {
    (
        channels: Vector[Channel],
        cachedContinuations: Vector[Continuation],
        installedContinuation: Continuation
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- state.modify(
                  _ =>
                    Cache(
                      continuations = TrieMap(
                        channels -> cachedContinuations
                      )
                    )
                )
            _     <- hotStore.installContinuation(channels, installedContinuation)
            res   <- hotStore.getContinuations(channels)
            cache <- state.read
            _ <- S.delay(
                  res shouldEqual installedContinuation +: cachedContinuations
                )
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

  "installContinuation" should "cache installed continuations separately" in forAll {
    (
        channels: Vector[Channel],
        cachedContinuations: Vector[Continuation],
        insertedContinuation: Continuation,
        installedContinuation: Continuation
    ) =>
      whenever(insertedContinuation != installedContinuation) {
        fixture { (state, history, hotStore) =>
          {
            for {
              _ <- state.modify(
                    _ =>
                      Cache(
                        continuations = TrieMap(
                          channels -> cachedContinuations
                        )
                      )
                  )
              _     <- hotStore.installContinuation(channels, installedContinuation)
              _     <- hotStore.putContinuation(channels, insertedContinuation)
              cache <- state.read
              _ <- S.delay(
                    cache
                      .installedContinuations(channels) shouldEqual installedContinuation
                  )
              _ <- S.delay(
                    cache
                      .continuations(channels) shouldEqual insertedContinuation +: cachedContinuations
                  )
            } yield ()
          }
        }
      }
  }

  "removeContinuation when cache is empty" should "read from history and remove the continuation from loaded data" in forAll {
    (
        channels: Vector[Channel],
        historyContinuations: Vector[Continuation],
        index: Int
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _     <- history.putContinuations(channels, historyContinuations)
            res   <- hotStore.removeContinuation(channels, index).attempt
            cache <- state.read
            _     <- checkRemoval(res, cache.continuations(channels), historyContinuations, index)
          } yield ()
        }
      }
  }

  "removeContinuation when cache contains data" should "read from the cache and remove the continuation from loaded data" in forAll {
    (
        channels: Vector[Channel],
        historyContinuations: Vector[Continuation],
        cachedContinuations: Vector[Continuation],
        index: Int
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
            res   <- hotStore.removeContinuation(channels, index).attempt
            cache <- state.read
            _     <- checkRemoval(res, cache.continuations(channels), cachedContinuations, index)
          } yield ()
        }
      }
  }

  "removeContinuation when installed continuation is present" should "not allow it's removal" in forAll {
    (
        channels: Vector[Channel],
        cachedContinuations: Vector[Continuation],
        installedContinuation: Continuation,
        index: Int
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- state.modify(
                  _ =>
                    Cache(
                      continuations = TrieMap(
                        channels -> cachedContinuations
                      ),
                      installedContinuations = TrieMap(
                        channels -> installedContinuation
                      )
                    )
                )
            res   <- hotStore.removeContinuation(channels, index).attempt
            cache <- state.read
            _ <- if (index == 0)
                  Sync[F].delay {
                    res shouldBe a[Left[IllegalArgumentException, _]]
                  } else
                  // index of the removed continuation includes the installed
                  hotStore.getContinuations(channels) >>= { continuations =>
                    checkRemoval(
                      res,
                      continuations,
                      installedContinuation +: cachedContinuations,
                      index
                    )
                  }

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

  "putDatum when cache is empty" should "read from history and add to it" in forAll {
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

  "putDatum when cache contains data" should "read from the cache and add to it" in forAll {
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

  "removeDatum when cache is empty" should "read from history and remove datum at index" in forAll {
    (
        channel: Channel,
        historyData: Vector[Data],
        index: Int
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _     <- history.putData(channel, historyData)
            res   <- hotStore.removeDatum(channel, index).attempt
            cache <- state.read
            _     <- checkRemoval(res, cache.data(channel), historyData, index)
          } yield ()
        }
      }
  }

  "removeDatum when cache contains data" should "read from the cache and remove datum" in forAll {
    (
        channel: Channel,
        historyData: Vector[Data],
        cachedData: Vector[Data],
        index: Int
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
            res   <- hotStore.removeDatum(channel, index).attempt
            cache <- state.read
            _     <- checkRemoval(res, cache.data(channel), cachedData, index)
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

  "putJoin when cache is empty" should "read from history and add to it" in forAll {
    (
        channel: Channel,
        historyJoins: Joins,
        insertedJoin: Join
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _     <- history.putJoins(channel, historyJoins)
            _     <- hotStore.putJoin(channel, insertedJoin)
            cache <- state.read
            _     <- S.delay(cache.joins(channel) shouldEqual insertedJoin +: historyJoins)
          } yield ()
        }
      }
  }

  "putJoin when cache contains data" should "read from the cache and add to it" in forAll {
    (
        channel: Channel,
        historyJoins: Joins,
        cachedJoins: Joins,
        insertedJoin: Join
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
            _     <- hotStore.putJoin(channel, insertedJoin)
            cache <- state.read
            _     <- S.delay(cache.joins(channel) shouldEqual insertedJoin +: cachedJoins)
          } yield ()
        }
      }
  }

  "installJoin" should "cache installed joins separately" in forAll {
    (
        channel: Channel,
        cachedJoins: Joins,
        insertedJoin: Join,
        installedJoin: Join
    ) =>
      whenever(insertedJoin != installedJoin) {
        fixture { (state, history, hotStore) =>
          {
            for {
              _ <- state.modify(
                    _ =>
                      Cache(
                        joins = TrieMap(
                          channel -> cachedJoins
                        )
                      )
                  )
              _     <- hotStore.putJoin(channel, insertedJoin)
              _     <- hotStore.installJoin(channel, installedJoin)
              cache <- state.read
              _ <- S.delay {
                    cache
                      .installedJoins(channel) shouldEqual Seq(installedJoin)
                  }
              _ <- S.delay(
                    cache
                      .joins(channel) shouldEqual insertedJoin +: cachedJoins
                  )
            } yield ()
          }
        }
      }
  }

  val arbitraryNonEmptyVectorOfJoins =
    Gen.nonEmptyContainerOf[Vector, Join](Arbitrary.arbitrary[Join])

  it should "allow installing multiple values per channel" in forAll(
    Arbitrary.arbitrary[String],
    arbitraryNonEmptyVectorOfJoins,
    arbitraryNonEmptyVectorOfJoins
  ) {
    (
        channel: Channel,
        cachedJoins: Joins,
        installedJoins: Joins
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- state.modify(
                  _ =>
                    Cache(
                      joins = TrieMap(
                        channel -> cachedJoins
                      )
                    )
                )
            _     <- installedJoins.map(j => hotStore.installJoin(channel, j)).sequence
            cache <- state.read
            _ <- S.delay {
                  cache
                    .installedJoins(channel) should contain theSameElementsAs (installedJoins)
                }
          } yield ()
        }
      }
  }

  "removeJoin when cache is empty" should "read from history and remove join" in forAll {
    (
        channel: Channel,
        historyJoins: Joins,
        index: Int,
        join: Join
    ) =>
      whenever(!historyJoins.contains(join)) {
        fixture { (state, history, hotStore) =>
          for {
            _        <- history.putJoins(channel, historyJoins)
            toRemove = historyJoins.get(index.toLong).getOrElse(join)
            res      <- hotStore.removeJoin(channel, toRemove).attempt
            cache    <- state.read
            _        <- checkRemoval(res, cache.joins(channel), historyJoins, index)
          } yield ()
        }
      }
  }

  "removeJoin when cache contains data" should "read from the cache and remove join" in forAll {
    (
        channel: Channel,
        historyJoins: Joins,
        cachedJoins: Joins,
        index: Int,
        join: Join
    ) =>
      whenever(!cachedJoins.contains(join)) {
        fixture { (state, history, hotStore) =>
          {
            for {
              _        <- history.putJoins(channel, historyJoins)
              toRemove = cachedJoins.get(index.toLong).getOrElse(join)
              _ <- state.modify(
                    _ =>
                      Cache(
                        joins = TrieMap(
                          channel -> cachedJoins
                        )
                      )
                  )
              res   <- hotStore.removeJoin(channel, toRemove).attempt
              cache <- state.read
              _     <- checkRemoval(res, cache.joins(channel), cachedJoins, index)
            } yield ()
          }
        }
      }
  }

  "removeJoin when installed joins are present" should "not allow removing them" in forAll {
    (
        channel: Channel,
        cachedJoins: Joins,
        installedJoins: Joins
    ) =>
      whenever(cachedJoins =!= installedJoins && installedJoins.nonEmpty) {
        fixture { (state, history, hotStore) =>
          {
            for {
              _ <- state.modify(
                    _ =>
                      Cache(
                        joins = TrieMap(
                          channel -> cachedJoins
                        ),
                        installedJoins = TrieMap(
                          channel -> installedJoins
                        )
                      )
                  )
              toRemove = Random.shuffle(installedJoins).head
              res      <- hotStore.removeJoin(channel, toRemove).attempt
              cache    <- state.read
              _ <- if (!cachedJoins.contains(toRemove))
                    Sync[F].delay {
                      res shouldBe a[Left[_, _]]
                    } else
                    for {
                      cache <- state.read
                      _ = cache.joins(channel).count(_ == toRemove) shouldEqual (cachedJoins.count(
                        _ == toRemove
                      ) - 1)
                      _ = cache.installedJoins(channel) shouldBe installedJoins
                    } yield ()
            } yield ()
          }
        }
      }
  }

  "changes" should "return information to be persisted in history" in forAll {
    (
        channels: Vector[Channel],
        channel: Channel,
        continuations: Vector[Continuation],
        installedContinuation: Continuation,
        data: Vector[Data],
        joins: Joins
    ) =>
      fixture { (state, _, hotStore) =>
        {
          for {
            _ <- state.modify(
                  _ =>
                    Cache(
                      continuations = TrieMap(
                        channels -> continuations
                      ),
                      installedContinuations = TrieMap(
                        channels -> installedContinuation
                      ),
                      data = TrieMap(
                        channel -> data
                      ),
                      joins = TrieMap(
                        channel -> joins
                      )
                    )
                )
            res   <- hotStore.changes()
            cache <- state.read
            _ <- S.delay {
                  res.size shouldBe (cache.continuations.size + cache.data.size + cache.joins.size)
                  if (continuations.isEmpty) res should contain(DeleteContinuations(channels))
                  else res should contain(InsertContinuations(channels, continuations))
                  if (data.isEmpty) res should contain(DeleteData(channel))
                  else res should contain(InsertData(channel, data))
                  if (joins.isEmpty) res should contain(DeleteJoins(channel))
                  else res should contain(InsertJoins(channel, joins))
                }
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

  private def checkRemoval[T](
      res: Either[Throwable, Unit],
      actual: Seq[T],
      initial: Seq[T],
      index: Int
  ): F[Assertion] = S.delay {
    if (index < 0 || index >= initial.size) {
      res shouldBe a[Left[_, _]]
      actual shouldEqual initial
    } else {
      res shouldBe a[Right[_, _]]
      actual shouldEqual initial.zipWithIndex
        .filter { case (_, i) => i != index }
        .map(_._1)
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
