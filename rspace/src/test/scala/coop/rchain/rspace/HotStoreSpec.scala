package coop.rchain.rspace

import cats.Parallel
import cats.effect.{Async, IO, Sync}
import cats.syntax.all._
import coop.rchain.rspace.examples.StringExamples.{StringsCaptor, _}
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.HistoryReaderBase
import coop.rchain.rspace.internal._
import coop.rchain.rspace.test.ArbitraryInstances._
import coop.rchain.shared.GeneratorUtils._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck._
import scodec.bits.ByteVector

import scala.collection.SortedSet
import scala.concurrent.duration._
import scala.util.Random
import cats.effect.Ref

trait HotStoreSpec[F[_]] extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 0, sizeRange = 10, minSuccessful = 20)

  implicit def S: Sync[F]
  implicit def P: Parallel[F]

  type Channel      = String
  type Data         = Datum[String]
  type Continuation = WaitingContinuation[Pattern, StringsCaptor]
  type Join         = Vector[Channel]
  type Joins        = Vector[Join]

  implicit val arbitraryJoins = distinctListOf[Join].map(_.toVector)

  implicit def arbitraryHotStoreState
      : Arbitrary[HotStoreState[String, Pattern, String, StringsCaptor]] =
    Arbitrary(
      for {

        continuations <- Arbitrary
                          .arbitrary[Map[Seq[String], Seq[
                            WaitingContinuation[Pattern, StringsCaptor]
                          ]]]
        installedContinuations <- Arbitrary
                                   .arbitrary[
                                     Map[Seq[String], WaitingContinuation[Pattern, StringsCaptor]]
                                   ]
        data           <- Arbitrary.arbitrary[Map[String, Seq[Datum[String]]]]
        joins          <- Arbitrary.arbitrary[Map[String, Seq[Seq[String]]]]
        installedJoins <- Arbitrary.arbitrary[Map[String, Seq[Seq[String]]]]
      } yield HotStoreState(
        continuations,
        installedContinuations,
        data,
        joins,
        installedJoins
      )
    )

  def fixture(
      f: (
          Ref[F, HotStoreState[String, Pattern, String, StringsCaptor]],
          History[F, String, Pattern, String, StringsCaptor],
          HotStore[F, String, Pattern, String, StringsCaptor]
      ) => F[Unit]
  ): Unit

  def fixture(cache: HotStoreState[String, Pattern, String, StringsCaptor])(
      f: HotStore[F, String, Pattern, String, StringsCaptor] => F[Unit]
  ): Unit

  "getContinuations when cache is empty" should "read from history and put into the cache" in forAll {
    (channels: Vector[Channel], historyContinuations: Vector[Continuation]) =>
      fixture { (state, history, hotStore) =>
        for {
          _                 <- history.putContinuations(channels, historyContinuations)
          cache             <- state.get
          _                 <- S.delay(cache.continuations shouldBe empty)
          readContinuations <- hotStore.getContinuations(channels)
          cache             <- state.get
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
            _                 <- history.putContinuations(channels, historyContinuations)
            _                 <- state.set(HotStoreState(continuations = Map(channels -> cachedContinuations)))
            readContinuations <- hotStore.getContinuations(channels)

            cache <- state.get
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
      fixture { (state, _, hotStore) =>
        {
          for {
            _   <- state.set(HotStoreState(continuations = Map(channels -> cachedContinuations)))
            _   <- hotStore.installContinuation(channels, installedContinuation)
            res <- hotStore.getContinuations(channels)
            _   <- state.get
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

            cache <- state.get
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
            _     <- history.putContinuations(channels, historyContinuations)
            _     <- state.set(HotStoreState(continuations = Map(channels -> cachedContinuations)))
            _     <- hotStore.putContinuation(channels, insertedContinuation)
            cache <- state.get
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
        fixture { (state, _, hotStore) =>
          {
            for {
              _     <- state.set(HotStoreState(continuations = Map(channels -> cachedContinuations)))
              _     <- hotStore.installContinuation(channels, installedContinuation)
              _     <- hotStore.putContinuation(channels, insertedContinuation)
              cache <- state.get
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
            cache <- state.get
            _ <- checkRemovalWorksOrFailsOnError(
                  res,
                  cache.continuations.getOrElse(channels, Seq.empty),
                  historyContinuations,
                  index
                )
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
            _     <- history.putContinuations(channels, historyContinuations)
            _     <- state.set(HotStoreState(continuations = Map(channels -> cachedContinuations)))
            res   <- hotStore.removeContinuation(channels, index).attempt
            cache <- state.get
            _ <- checkRemovalWorksOrFailsOnError(
                  res,
                  cache.continuations(channels),
                  cachedContinuations,
                  index
                )
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
      fixture { (state, _, hotStore) =>
        {
          for {
            _ <- state.set(
                  HotStoreState(
                    continuations = Map(channels          -> cachedContinuations),
                    installedContinuations = Map(channels -> installedContinuation)
                  )
                )
            res <- hotStore.removeContinuation(channels, index).attempt
            _   <- state.get
            _ <- if (index == 0)
                  Sync[F].delay {
                    res shouldBe a[Left[IllegalArgumentException, _]]
                  } else
                  // index of the removed continuation includes the installed
                  hotStore.getContinuations(channels) >>= { continuations =>
                    checkRemovalWorksOrFailsOnError(
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
          cache    <- state.get
          _        = cache.data shouldBe empty
          readData <- hotStore.getData(channel)
          cache    <- state.get
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
            _        <- history.putData(channel, historyData)
            _        <- state.set(HotStoreState(data = Map(channel -> cachedData)))
            readData <- hotStore.getData(channel)
            cache    <- state.get
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
            cache <- state.get
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
            _     <- history.putData(channel, historyData)
            _     <- state.set(HotStoreState(data = Map(channel -> cachedData)))
            _     <- hotStore.putDatum(channel, insertedData)
            cache <- state.get
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
            cache <- state.get
            _ <- checkRemovalWorksOrFailsOnError(
                  res,
                  cache.data.getOrElse(channel, Seq.empty),
                  historyData,
                  index
                )
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
            _     <- history.putData(channel, historyData)
            _     <- state.set(HotStoreState(data = Map(channel -> cachedData)))
            res   <- hotStore.removeDatum(channel, index).attempt
            cache <- state.get
            _     <- checkRemovalWorksOrFailsOnError(res, cache.data(channel), cachedData, index)
          } yield ()
        }
      }
  }

  "getJoins when cache is empty" should "read from history and put into the cache" in forAll {
    (channel: Channel, historyJoins: Vector[Vector[Channel]]) =>
      fixture { (state, history, hotStore) =>
        for {
          _         <- history.putJoins(channel, historyJoins)
          cache     <- state.get
          _         = cache.joins shouldBe empty
          readJoins <- hotStore.getJoins(channel)
          cache     <- state.get
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
            _         <- history.putJoins(channel, historyJoins)
            _         <- state.set(HotStoreState(joins = Map(channel -> cachedJoins)))
            readJoins <- hotStore.getJoins(channel)
            cache     <- state.get
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
      whenever(!historyJoins.contains(insertedJoin)) {
        fixture { (state, history, hotStore) =>
          {
            for {
              _     <- history.putJoins(channel, historyJoins)
              _     <- hotStore.putJoin(channel, insertedJoin)
              cache <- state.get
              _     <- S.delay(cache.joins(channel) shouldEqual insertedJoin +: historyJoins)
            } yield ()
          }
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
      whenever(!cachedJoins.contains(insertedJoin)) {
        fixture { (state, history, hotStore) =>
          {
            for {
              _     <- history.putJoins(channel, historyJoins)
              _     <- state.set(HotStoreState(joins = Map(channel -> cachedJoins)))
              _     <- hotStore.putJoin(channel, insertedJoin)
              cache <- state.get
              _ <- S.delay {
                    cache.joins(channel) shouldEqual insertedJoin +: cachedJoins
                  }
            } yield ()
          }
        }
      }
  }

  "putJoin" should "not allow inserting duplicate joins" in forAll {
    (
        channel: Channel,
        cachedJoins: Joins,
        insertedJoin: Join
    ) =>
      fixture { (state, _, hotStore) =>
        {
          for {
            _     <- state.set(HotStoreState(joins = Map(channel -> cachedJoins)))
            _     <- hotStore.putJoin(channel, insertedJoin)
            cache <- state.get
            _ <- S.delay {
                  if (!cachedJoins.contains(insertedJoin))
                    cache.joins(channel) shouldEqual insertedJoin +: cachedJoins
                  else
                    cache.joins(channel) shouldEqual cachedJoins
                }
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
      whenever(insertedJoin != installedJoin && !cachedJoins.contains(insertedJoin)) {
        fixture { (state, _, hotStore) =>
          {
            for {
              _     <- state.set(HotStoreState(joins = Map(channel -> cachedJoins)))
              _     <- hotStore.putJoin(channel, insertedJoin)
              _     <- hotStore.installJoin(channel, installedJoin)
              cache <- state.get
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

  it should "not allow installing duplicate joins per channel" in forAll {
    (
        channel: Channel,
        cachedJoins: Joins,
        installedJoin: Join
    ) =>
      fixture { (state, _, hotStore) =>
        {
          for {
            _     <- state.set(HotStoreState(joins = Map(channel -> cachedJoins)))
            _     <- hotStore.installJoin(channel, installedJoin)
            _     <- hotStore.installJoin(channel, installedJoin)
            cache <- state.get
            _ <- S.delay {
                  cache
                    .installedJoins(channel) shouldBe Vector(installedJoin)
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
            cache    <- state.get
            _ <- checkRemovalWorksOrIgnoresErrors(
                  res,
                  cache.joins(channel),
                  historyJoins,
                  index
                )
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
              _        <- state.set(HotStoreState(joins = Map(channel -> cachedJoins)))
              res      <- hotStore.removeJoin(channel, toRemove).attempt
              cache    <- state.get
              _        <- checkRemovalWorksOrIgnoresErrors(res, cache.joins(channel), cachedJoins, index)
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
        fixture { (state, _, hotStore) =>
          {
            for {
              _ <- state.set(
                    HotStoreState(
                      joins = Map(channel          -> cachedJoins),
                      installedJoins = Map(channel -> installedJoins)
                    )
                  )
              toRemove = Random.shuffle(installedJoins).head
              res      <- hotStore.removeJoin(channel, toRemove).attempt
              cache    <- state.get
              _ <- if (!cachedJoins.contains(toRemove))
                    Sync[F].delay {
                      res shouldBe a[Right[_, _]]
                      cache.joins(channel) shouldEqual cachedJoins
                    } else
                    for {
                      cache <- state.get
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

  "removeJoin" should "not remove a join when a continuation is present" in forAll(
    Arbitrary.arbitrary[Channel],
    Arbitrary.arbitrary[Continuation],
    arbitraryNonEmptyVectorOfJoins
  ) {
    (
        channel: Channel,
        continuation: Continuation,
        cachedJoins: Joins
    ) =>
      fixture { (state, _, hotStore) =>
        {
          for {
            _        <- state.set(HotStoreState(joins = Map(channel -> cachedJoins)))
            toRemove = Random.shuffle(cachedJoins).head
            _        <- hotStore.putContinuation(toRemove, continuation)
            res      <- hotStore.removeJoin(channel, toRemove).attempt
            cache    <- state.get
            _ <- Sync[F].delay {
                  res shouldBe a[Right[_, _]]
                  cache.joins(channel) shouldEqual cachedJoins
                }
          } yield ()
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
            _ <- state.set(
                  HotStoreState(
                    continuations = Map(channels          -> continuations),
                    installedContinuations = Map(channels -> installedContinuation),
                    data = Map(channel                    -> data),
                    joins = Map(channel                   -> joins)
                  )
                )
            res   <- hotStore.changes
            cache <- state.get
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
        fixture { (_, history, hotStore) =>
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

  "concurrent continuation operations on disjoint channels" should "not mess up the cache" in forAll(
    Gen.nonEmptyContainerOf[Vector, Channel](Arbitrary.arbitrary[Channel]),
    Gen.nonEmptyContainerOf[Vector, Channel](Arbitrary.arbitrary[Channel]),
    Gen.containerOfN[Vector, Continuation](10, Arbitrary.arbitrary[Continuation]),
    Gen.containerOfN[Vector, Continuation](10, Arbitrary.arbitrary[Continuation]),
    Arbitrary.arbitrary[Continuation],
    Arbitrary.arbitrary[Continuation]
  ) {
    (
        channels1: Vector[Channel],
        channels2: Vector[Channel],
        historyContinuations1: Vector[Continuation],
        historyContinuations2: Vector[Continuation],
        insertedContinuation1: Continuation,
        insertedContinuation2: Continuation
    ) =>
      whenever(channels1 =!= channels2) {
        fixture { (_, history, hotStore) =>
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

  private def checkRemovalWorksOrFailsOnError[T](
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

  private def checkRemovalWorksOrIgnoresErrors[T](
      res: Either[Throwable, Unit],
      actual: Seq[T],
      initial: Seq[T],
      index: Int
  ): F[Assertion] = S.delay {
    if (index < 0 || index >= initial.size) {
      res shouldBe a[Right[_, _]]
      actual shouldEqual initial
    } else {
      res shouldBe a[Right[_, _]]
      actual shouldEqual initial.zipWithIndex
        .filter { case (_, i) => i != index }
        .map(_._1)
    }
  }

  /*
   *  The tests below were migrated from IStoreTests to check backward compatibility
   */

  "putDatum" should "put datum in a new channel" in forAll("channel", "datum") {
    (channel: String, datumValue: String) =>
      fixture { (_, _, hotStore) =>
        val key   = channel
        val datum = Datum.create(channel, datumValue, persist = false)

        for {
          _   <- hotStore.putDatum(key, datum)
          res <- hotStore.getData(key)
          _   <- S.delay { res should contain theSameElementsAs Seq(datum) }
        } yield ()
      }
  }

  it should "append datum if channel already exists" in forAll("channel", "datum") {
    (channel: String, datumValue: String) =>
      fixture { (_, _, store) =>
        val key    = channel
        val datum1 = Datum.create(channel, datumValue, persist = false)
        val datum2 = Datum.create(channel, datumValue + "2", persist = false)

        for {
          _   <- store.putDatum(key, datum1)
          _   <- store.putDatum(key, datum2)
          res <- store.getData(key)
          _   <- S.delay(res should contain theSameElementsAs Seq(datum1, datum2))
        } yield ()
      }
  }

  private[this] val validIndices =
    for (n <- Gen.choose(1, 10)) yield n
  private[this] val size: Int = 11

  "removeDatum" should s"remove datum at index" in
    forAll("channel", "datum", validIndices, minSuccessful(10)) {
      (channel: String, datumValue: String, index: Int) =>
        fixture { (_, _, store) =>
          val key = channel
          val data = List.tabulate(size) { i =>
            Datum.create(channel, datumValue + i, persist = false)
          }

          for {
            _ <- data.map { d =>
                  store.putDatum(key, d)
                }.sequence
            _   <- store.removeDatum(key, index - 1)
            res <- store.getData(key)
            _ <- S.delay {
                  res should contain theSameElementsAs data.filterNot(
                    _.a == datumValue + (size - index)
                  )
                }
          } yield ()
        }
    }

  "putWaitingContinuation" should "put waiting continuation in a new channel" in
    forAll("channel", "continuation") { (channel: String, pattern: String) =>
      fixture { (_, _, store) =>
        val key          = collection.immutable.Seq(channel)
        val patterns     = List(StringMatch(pattern))
        val continuation = new StringsCaptor
        val wc: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation.create(key, patterns, continuation, persist = false, SortedSet.empty)

        for {
          _   <- store.putContinuation(key, wc)
          res <- store.getContinuations(key)
          _   <- S.delay(res shouldBe List(wc))
        } yield ()
      }
    }

  it should "append continuation if channel already exists" in
    forAll("channel", "continuation") { (channel: String, pattern: String) =>
      fixture { (_, _, store) =>
        val key          = List(channel)
        val patterns     = List(StringMatch(pattern))
        val continuation = new StringsCaptor
        val wc1: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation.create(key, patterns, continuation, persist = false, SortedSet.empty)

        val wc2: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation.create(
            key,
            List(StringMatch(pattern + 2)),
            continuation,
            persist = false,
            SortedSet.empty
          )

        for {
          _   <- store.putContinuation(key, wc1)
          _   <- store.putContinuation(key, wc2)
          res <- store.getContinuations(key)
          _   <- S.delay(res should contain theSameElementsAs List(wc1, wc2))
        } yield ()
      }
    }

  "removeWaitingContinuation" should "remove waiting continuation from index" in
    forAll("channel", "continuation") { (channel: String, pattern: String) =>
      fixture { (_, _, store) =>
        val key          = List(channel)
        val patterns     = List(StringMatch(pattern))
        val continuation = new StringsCaptor
        val wc1: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation.create(key, patterns, continuation, persist = false, SortedSet.empty)
        val wc2: WaitingContinuation[Pattern, StringsCaptor] =
          WaitingContinuation.create(
            key,
            List(StringMatch(pattern + 2)),
            continuation,
            persist = false,
            SortedSet.empty
          )

        for {
          _   <- store.putContinuation(key, wc1)
          _   <- store.putContinuation(key, wc2)
          _   <- store.removeContinuation(key, 0)
          res <- store.getContinuations(key)
          _   <- S.delay(res shouldBe List(wc1))
        } yield ()
      }
    }

  "addJoin" should "add join for a channel" in
    forAll("channel", "channels") { (channel: String, channels: List[String]) =>
      fixture { (_, _, store) =>
        for {
          _   <- store.putJoin(channel, channels)
          res <- store.getJoins(channel)
          _   <- S.delay(res shouldBe List(channels))
        } yield ()
      }
    }

  "removeJoin" should "remove join for a channel" in
    forAll("channel", "channels") { (channel: String, channels: List[String]) =>
      fixture { (_, _, store) =>
        for {
          _   <- store.putJoin(channel, channels)
          _   <- store.removeJoin(channel, channels)
          res <- store.getJoins(channel)
          _   <- S.delay(res shouldBe empty)
        } yield ()
      }
    }

  it should "remove only passed in joins for a channel" in
    forAll("channel", "channels") { (channel: String, channels: List[String]) =>
      fixture { (_, _, store) =>
        for {
          _   <- store.putJoin(channel, channels)
          _   <- store.putJoin(channel, List("otherChannel"))
          _   <- store.removeJoin(channel, channels)
          res <- store.getJoins(channel)
          _   <- S.delay(res shouldBe List(List("otherChannel")))
        } yield ()
      }
    }

  "snapshot" should "create a copy of the cache" in forAll {
    (cache: HotStoreState[String, Pattern, String, StringsCaptor]) =>
      fixture(cache) { store =>
        for {
          snapshot <- store.snapshot
        } yield snapshot shouldBe cache
      }
  }

  it should "create a deep copy of the continuations in the cache" in forAll {
    (
        channels: Vector[Channel],
        continuation1: Continuation,
        continuation2: Continuation
    ) =>
      whenever(continuation1 != continuation2) {
        fixture { (_, _, store) =>
          for {
            _        <- store.putContinuation(channels, continuation1)
            snapshot <- store.snapshot
            _        <- store.putContinuation(channels, continuation2)
            _        = snapshot.continuations(channels) should contain(continuation1)
          } yield snapshot.continuations(channels) should not contain continuation2
        }
      }
  }

  it should "create a deep copy of the installed continuations in the cache" in forAll {
    (
        channels: Vector[Channel],
        continuation1: Continuation,
        continuation2: Continuation
    ) =>
      whenever(continuation1 != continuation2) {
        fixture { (_, _, store) =>
          for {
            _        <- store.installContinuation(channels, continuation1)
            snapshot <- store.snapshot
            _        <- store.installContinuation(channels, continuation2)
          } yield snapshot.installedContinuations(channels) shouldBe continuation1
        }
      }
  }

  it should "create a deep copy of the data in the cache" in forAll {
    (
        channel: Channel,
        data1: Data,
        data2: Data
    ) =>
      whenever(data1 != data2) {
        fixture { (_, _, store) =>
          for {
            _        <- store.putDatum(channel, data1)
            snapshot <- store.snapshot
            _        <- store.putDatum(channel, data2)
            _        = snapshot.data(channel) should contain(data1)
          } yield snapshot.data(channel) should not contain data2
        }
      }
  }

  it should "create a deep copy of the joins in the cache" in forAll {
    (
        channel: Channel,
        join1: Join,
        join2: Join
    ) =>
      whenever(join1 != join2) {
        fixture { (_, _, store) =>
          for {
            _        <- store.putJoin(channel, join1)
            snapshot <- store.snapshot
            _        <- store.putJoin(channel, join2)
            _        = snapshot.joins(channel) should contain(join1)
          } yield snapshot.joins(channel) should not contain join2
        }
      }
  }

  it should "create a deep copy of the installed joins in the cache" in forAll {
    (
        channel: Channel,
        join1: Join,
        join2: Join
    ) =>
      whenever(join1 != join2) {
        fixture { (_, _, store) =>
          for {
            _        <- store.installJoin(channel, join1)
            snapshot <- store.snapshot
            _        <- store.installJoin(channel, join2)
            _        = snapshot.installedJoins(channel) should contain(join1)
          } yield snapshot.installedJoins(channel) should not contain join2
        }
      }
  }
}

class History[F[_]: Sync, C, P, A, K](R: Ref[F, HotStoreState[C, P, A, K]])
    extends HistoryReaderBase[F, C, P, A, K] {

  override def getJoins(channel: C): F[Seq[Seq[C]]] =
    R.get.map(_.joins.get(channel).toSeq.flatten)
  def putJoins(channel: C, joins: Seq[Seq[C]]): F[Unit] = R.modify { prev =>
    (prev.copy(joins = prev.joins.updated(channel, joins)), ())
  }

  override def getData(channel: C): F[Seq[Datum[A]]] =
    R.get.map(_.data.get(channel).toSeq.flatten)
  def putData(channel: C, data: Seq[Datum[A]]): F[Unit] = R.modify { prev =>
    (prev.copy(data = prev.data.updated(channel, data)), ())
  }

  override def getContinuations(
      channels: Seq[C]
  ): F[Seq[WaitingContinuation[P, K]]] =
    R.get.map(_.continuations.get(channels).toSeq.flatten)
  def putContinuations(
      channels: Seq[C],
      continuations: Seq[WaitingContinuation[P, K]]
  ): F[Unit] = R.modify { prev =>
    (prev.copy(continuations = prev.continuations.updated(channels, continuations)), ())
  }

  // Not used in testing (instead defaults are implemented: getData, getContinuations, getJoins)
  override def getDataProj[R](key: C): ((Datum[A], ByteVector) => R) => F[Seq[R]] = ???
  override def getContinuationsProj[R](
      key: Seq[C]
  ): ((WaitingContinuation[P, K], ByteVector) => R) => F[Seq[R]]                 = ???
  override def getJoinsProj[R](key: C): ((Seq[C], ByteVector) => R) => F[Seq[R]] = ???
}

trait InMemHotStoreSpec extends HotStoreSpec[IO] {

  import coop.rchain.shared.RChainScheduler._
  protected type F[A] = IO[A]
  implicit override val S: Sync[F]      = implicitly[Async[IO]]
  implicit override val P: Parallel[IO] = IO.ioParallel
  def C(
      c: HotStoreState[String, Pattern, String, StringsCaptor] = HotStoreState()
  ): F[Ref[F, HotStoreState[String, Pattern, String, StringsCaptor]]]

  override def fixture(
      f: (
          Ref[F, HotStoreState[String, Pattern, String, StringsCaptor]],
          History[F, String, Pattern, String, StringsCaptor],
          HotStore[F, String, Pattern, String, StringsCaptor]
      ) => F[Unit]
  ) =
    (for {
      historyState <- Ref[F].of(HotStoreState[String, Pattern, String, StringsCaptor]())
      history      = new History[F, String, Pattern, String, StringsCaptor](historyState)
      cache        <- C()
      hotStore     <- HotStore[F, String, Pattern, String, StringsCaptor](cache, history)
      res          <- f(cache, history, hotStore)
    } yield res).unsafeRunTimed(1.second)

  override def fixture(cache: HotStoreState[String, Pattern, String, StringsCaptor])(
      f: HotStore[F, String, Pattern, String, StringsCaptor] => F[Unit]
  ) =
    (for {
      historyState <- Ref[F].of(HotStoreState[String, Pattern, String, StringsCaptor]())
      history      = new History[F, String, Pattern, String, StringsCaptor](historyState)
      cache        <- C(cache)
      hotStore     <- HotStore[F, String, Pattern, String, StringsCaptor](cache, history)
      res          <- f(hotStore)
    } yield res).unsafeRunTimed(1.second)

}

class RefHotStoreStatedInMemHotStoreSpec extends InMemHotStoreSpec {
  implicit override def C(
      cache: HotStoreState[String, Pattern, String, StringsCaptor]
  ): F[Ref[F, HotStoreState[String, Pattern, String, StringsCaptor]]] =
    Ref[F].of(cache)

}
