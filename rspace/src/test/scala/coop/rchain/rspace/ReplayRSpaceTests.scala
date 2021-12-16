package coop.rchain.rspace

import cats.Functor
import cats.effect.concurrent.Ref
import cats.syntax.all._
import cats.effect._
import cats.effect.concurrent.{Ref, Semaphore}
import com.typesafe.scalalogging.Logger
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.ski._
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.HistoryRepositoryInstances
import coop.rchain.rspace.test._
import coop.rchain.rspace.trace.Consume
import coop.rchain.rspace.util.ReplayException
import coop.rchain.shared.{Log, Serialize}
import coop.rchain.store.InMemoryStoreManager
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.AtomicAny
import org.scalacheck._
import org.scalatest._
import org.scalatest.prop._

import scala.collection.SortedSet
import scala.util.Random
import scala.util.Random.shuffle

object SchedulerPools {
  implicit val global = Scheduler.fixedPool("GlobalPool", 20)
  val rspacePool      = Scheduler.fixedPool("RSpacePool", 5)
}

//noinspection ZeroIndexToHead,NameBooleanParameters
trait ReplayRSpaceTests extends ReplayRSpaceTestsBase[String, Pattern, String, String] {
  import SchedulerPools.global
  import cats.syntax.parallel._

  implicit val log: Log[Task]      = new Log.NOPLog[Task]
  val arbitraryRangeSize: Gen[Int] = Gen.chooseNum[Int](1, 10)
  val arbitraryRangesSize: Gen[(Int, Int)] = for {
    m <- Gen.chooseNum[Int](1, 10)
    n <- Gen.chooseNum[Int](1, m)
  } yield (n, m)

  def consumeMany[C, P, A, K](
      space: ISpace[Task, C, P, A, K],
      range: Seq[Int],
      channelsCreator: Int => List[C],
      patterns: List[P],
      continuationCreator: Int => K,
      persist: Boolean,
      peeks: SortedSet[Int] = SortedSet.empty
  ): Task[List[Option[(ContResult[C, P, K], Seq[Result[C, A]])]]] =
    shuffle(range).toList.parTraverse { i: Int =>
      logger.debug("Started consume {}", i)
      space
        .consume(channelsCreator(i), patterns, continuationCreator(i), persist, peeks = peeks)
        .map { r =>
          logger.debug("Finished consume {}", i)
          r
        }
    }

  def produceMany[C, P, A, K](
      space: ISpace[Task, C, P, A, K],
      range: Seq[Int],
      channelCreator: Int => C,
      datumCreator: Int => A,
      persist: Boolean
  ): Task[List[Option[(ContResult[C, P, K], Seq[Result[C, A]])]]] =
    shuffle(range).toList.parTraverse { i: Int =>
      logger.debug("Started produce {}", i)
      space.produce(channelCreator(i), datumCreator(i), persist).map { r =>
        logger.debug("Finished produce {}", i)
        r
      }
    }

  "reset to a checkpoint from a different branch" should "work" in fixture {
    (store, replayStore, space, replaySpace) =>
      for {
        root0 <- replaySpace.createCheckpoint().map(_.root)
        _     = replayStore.get().isEmpty.map(_ shouldBe true)

        _     <- space.produce("ch1", "datum1", false)
        root1 <- space.createCheckpoint().map(_.root)

        _ <- replaySpace.reset(root1)
        _ <- replayStore.get().isEmpty.map(_ shouldBe true)

        _ <- space.reset(root0)
        _ <- store.get().isEmpty.map(_ shouldBe true)
      } yield ()
  }

  "Creating a COMM Event" should "replay correctly" in
    fixture { (store, replayStore, space, replaySpace) =>
      val channels     = List("ch1")
      val patterns     = List(Wildcard)
      val continuation = "continuation"
      val datum        = "datum1"

      for {
        emptyPoint <- space.createCheckpoint()

        resultConsume <- space.consume(channels, patterns, continuation, false)
        resultProduce <- space.produce(channels(0), datum, false)
        rigPoint      <- space.createCheckpoint()

        _ = resultConsume shouldBe None
        _ = resultProduce shouldBe Some(
          (
            ContResult(continuation, false, channels, patterns),
            List(Result(channels(0), datum, datum, false))
          )
        )

        _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

        replayResultConsume <- replaySpace.consume(channels, patterns, continuation, false)
        replayResultProduce <- replaySpace.produce(channels(0), datum, false)
        finalPoint          <- replaySpace.createCheckpoint()

        _ = replayResultConsume shouldBe None
        _ = replayResultProduce shouldBe resultProduce
        _ = finalPoint.root shouldBe rigPoint.root
        _ = replaySpace.replayData shouldBe empty
      } yield ()
    }

  "Creating a COMM Event with peek consume first" should "replay correctly" in
    fixture { (store, replayStore, space, replaySpace) =>
      val channels     = List("ch1")
      val patterns     = List(Wildcard)
      val continuation = "continuation"
      val datum        = "datum1"

      for {
        emptyPoint <- space.createCheckpoint()

        resultConsume <- space.consume(
                          channels,
                          patterns,
                          continuation,
                          false,
                          peeks = SortedSet(0)
                        )
        resultProduce <- space.produce(channels(0), datum, false)
        rigPoint      <- space.createCheckpoint()

        _ = resultConsume shouldBe None
        _ = resultProduce shouldBe Some(
          (
            ContResult(continuation, false, channels, patterns, true),
            List(Result(channels(0), datum, datum, false))
          )
        )

        _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

        replayResultConsume <- replaySpace.consume(
                                channels,
                                patterns,
                                continuation,
                                false,
                                peeks = SortedSet(0)
                              )
        replayResultProduce <- replaySpace.produce(channels(0), datum, false)
        finalPoint          <- replaySpace.createCheckpoint()

        _ = replayResultConsume shouldBe None
        _ = replayResultProduce shouldBe resultProduce
        _ = finalPoint.root shouldBe rigPoint.root
        _ = replaySpace.replayData shouldBe empty
      } yield ()
    }

  "Creating a COMM Event with peek produce first" should "replay correctly" in
    fixture { (store, replayStore, space, replaySpace) =>
      val channels     = List("ch1")
      val patterns     = List(Wildcard)
      val continuation = "continuation"
      val datum        = "datum1"

      for {
        emptyPoint <- space.createCheckpoint()

        resultProduce <- space.produce(channels(0), datum, false)
        resultConsume <- space.consume(
                          channels,
                          patterns,
                          continuation,
                          false,
                          peeks = SortedSet(0)
                        )
        rigPoint <- space.createCheckpoint()

        _ = resultProduce shouldBe None
        _ = resultConsume shouldBe defined

        _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

        replayResultProduce <- replaySpace.produce(channels(0), datum, false)
        replayResultConsume <- replaySpace.consume(
                                channels,
                                patterns,
                                continuation,
                                false,
                                peeks = SortedSet(0)
                              )
        finalPoint <- replaySpace.createCheckpoint()

        _ = replayResultProduce shouldBe None
        _ = replayResultConsume shouldBe resultConsume
        _ = finalPoint.root shouldBe rigPoint.root
        _ = replaySpace.replayData shouldBe empty
      } yield ()
    }

  "Creating COMM Events on many channels with peek" should "replay correctly" in
    fixture { (store, replayStore, space, replaySpace) =>
      val channels     = List("ch1", "ch2")
      val patterns     = List(Wildcard, Wildcard)
      val continuation = "continuation"
      val datum        = "datum1"

      for {
        emptyPoint <- space.createCheckpoint()

        resultConsume1 <- space.consume(
                           channels,
                           patterns,
                           continuation,
                           false,
                           peeks = SortedSet(0)
                         )
        resultProduce1  <- space.produce(channels(1), datum, false)
        resultProduce2  <- space.produce(channels(0), datum, false)
        resultProduce2a <- space.produce(channels(0), datum, false)
        resultConsume2 <- space.consume(
                           channels,
                           patterns,
                           continuation,
                           false,
                           peeks = SortedSet(1)
                         )
        resultProduce3  <- space.produce(channels(1), datum, false)
        resultProduce3a <- space.produce(channels(1), datum, false)
        resultConsume3  <- space.consume(channels, patterns, continuation, false)
        resultProduce4  <- space.produce(channels(0), datum, false)

        rigPoint <- space.createCheckpoint()

        _ = resultConsume1 shouldBe None
        _ = resultProduce1 shouldBe None
        _ = resultProduce2 shouldBe defined
        _ = resultConsume2 shouldBe None
        _ = resultProduce3 shouldBe defined
        _ = resultConsume3 shouldBe None
        _ = resultProduce4 shouldBe defined

        _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

        replayConsume1 <- replaySpace.consume(
                           channels,
                           patterns,
                           continuation,
                           false,
                           peeks = SortedSet(0)
                         )
        replayProduce1  <- replaySpace.produce(channels(1), datum, false)
        replayProduce2  <- replaySpace.produce(channels(0), datum, false)
        replayProduce2a <- replaySpace.produce(channels(0), datum, false)
        replayConsume2 <- replaySpace.consume(
                           channels,
                           patterns,
                           continuation,
                           false,
                           peeks = SortedSet(1)
                         )
        replayProduce3  <- replaySpace.produce(channels(1), datum, false)
        replayProduce3a <- replaySpace.produce(channels(1), datum, false)
        replayConsume3  <- replaySpace.consume(channels, patterns, continuation, false)
        replayProduce4  <- replaySpace.produce(channels(0), datum, false)

        _ = replayConsume1 shouldBe None
        _ = replayProduce1 shouldBe None
        _ = replayProduce2 shouldBe defined
        _ = replayProduce2a shouldBe None
        _ = replayConsume2 shouldBe None
        _ = replayProduce3 shouldBe defined
        _ = replayProduce3a shouldBe None
        _ = replayConsume3 shouldBe None
        _ = replayProduce4 shouldBe defined

        finalPoint <- replaySpace.createCheckpoint()

        _ = finalPoint.root shouldBe rigPoint.root
        _ = replaySpace.replayData shouldBe empty
      } yield ()
    }

  "Creating multiple COMM Event with peeking a produce" should "replay correctly" in
    fixture { (store, replayStore, space, replaySpace) =>
      val channels     = List("ch1")
      val patterns     = List(Wildcard)
      val continuation = "continuation"
      val datum        = "datum1"

      for {
        emptyPoint <- space.createCheckpoint()

        resultConsume1 <- space.consume(
                           channels,
                           patterns,
                           continuation,
                           false,
                           peeks = SortedSet(0)
                         )
        resultProduce  <- space.produce(channels(0), datum, false)
        resultProduce2 <- space.produce(channels(0), datum, false)
        resultConsume2 <- space.consume(
                           channels,
                           patterns,
                           continuation,
                           false,
                           peeks = SortedSet(0)
                         )
        resultProduce3 <- space.produce(channels(0), datum, false)
        resultConsume3 <- space.consume(
                           channels,
                           patterns,
                           continuation,
                           false,
                           peeks = SortedSet(0)
                         )
        resultProduce4 <- space.produce(channels(0), datum, false)
        resultConsume4 <- space.consume(
                           channels,
                           patterns,
                           continuation,
                           false,
                           peeks = SortedSet(0)
                         )
        resultProduce5 <- space.produce(channels(0), datum, false)
        resultConsume5 <- space.consume(
                           channels,
                           patterns,
                           continuation,
                           false,
                           peeks = SortedSet(0)
                         )
        rigPoint <- space.createCheckpoint()

        expectedResult = Some(
          (
            ContResult(continuation, false, channels, patterns, true),
            List(Result(channels(0), datum, datum, false))
          )
        )
        _ = resultConsume1 shouldBe None
        _ = resultConsume2 shouldBe expectedResult
        _ = resultConsume3 shouldBe expectedResult
        _ = resultConsume4 shouldBe expectedResult
        _ = resultConsume5 shouldBe expectedResult
        _ = resultProduce shouldBe expectedResult
        _ = resultProduce2 shouldBe None
        _ = resultProduce3 shouldBe None
        _ = resultProduce4 shouldBe None
        _ = resultProduce5 shouldBe None
        _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

        replayResultConsume1 <- replaySpace.consume(
                                 channels,
                                 patterns,
                                 continuation,
                                 false,
                                 peeks = SortedSet(0)
                               )
        replayResultProduce  <- replaySpace.produce(channels(0), datum, false)
        replayResultProduce2 <- replaySpace.produce(channels(0), datum, false)
        replayResultConsume2 <- replaySpace.consume(
                                 channels,
                                 patterns,
                                 continuation,
                                 false,
                                 peeks = SortedSet(0)
                               )
        replayResultProduce3 <- replaySpace.produce(channels(0), datum, false)
        replayResultConsume3 <- replaySpace.consume(
                                 channels,
                                 patterns,
                                 continuation,
                                 false,
                                 peeks = SortedSet(0)
                               )
        replayResultProduce4 <- replaySpace.produce(channels(0), datum, false)
        replayResultConsume4 <- replaySpace.consume(
                                 channels,
                                 patterns,
                                 continuation,
                                 false,
                                 peeks = SortedSet(0)
                               )
        replayResultProduce5 <- replaySpace.produce(channels(0), datum, false)
        replayResultConsume5 <- replaySpace.consume(
                                 channels,
                                 patterns,
                                 continuation,
                                 false,
                                 peeks = SortedSet(0)
                               )
        finalPoint <- replaySpace.createCheckpoint()

        _ = replayResultConsume1 shouldBe resultConsume1
        _ = replayResultConsume2 shouldBe resultConsume2
        _ = replayResultConsume3 shouldBe resultConsume3
        _ = replayResultConsume4 shouldBe resultConsume4
        _ = replayResultConsume5 shouldBe resultConsume5
        _ = replayResultProduce shouldBe resultProduce
        _ = replayResultProduce2 shouldBe resultProduce2
        _ = replayResultProduce3 shouldBe resultProduce3
        _ = replayResultProduce4 shouldBe resultProduce4
        _ = replayResultProduce5 shouldBe resultProduce5
        _ = finalPoint.root shouldBe rigPoint.root
        _ = replaySpace.replayData shouldBe empty
      } yield ()
    }

  "Picking n datums from m waiting datums" should "replay correctly" in forAll(arbitraryRangesSize) {
    case (n: Int, m: Int) =>
      fixture { (store, replayStore, space, replaySpace) =>
        for {
          emptyPoint <- space.createCheckpoint()

          range = (n until m).toList

          _ <- produceMany(
                space,
                range,
                channelCreator = kp("ch1"),
                datumCreator = i => s"datum$i",
                persist = false
              )
          results <- consumeMany(
                      space,
                      range,
                      channelsCreator = kp(List("ch1")),
                      patterns = List(Wildcard),
                      continuationCreator = i => s"continuation$i",
                      persist = false
                    )
          rigPoint <- space.createCheckpoint()

          _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

          _ <- produceMany(
                replaySpace,
                range,
                channelCreator = kp("ch1"),
                datumCreator = i => s"datum$i",
                persist = false
              )
          replayResults <- consumeMany(
                            replaySpace,
                            range,
                            channelsCreator = kp(List("ch1")),
                            patterns = List(Wildcard),
                            continuationCreator = i => s"continuation$i",
                            persist = false
                          )
          finalPoint <- replaySpace.createCheckpoint()

          _ = replayResults should contain theSameElementsAs results
          _ = finalPoint.root shouldBe rigPoint.root
          _ = replaySpace.replayData shouldBe empty
        } yield ()
      }
  }

  "A matched continuation defined for multiple channels, some peeked" should "replay correctly" in forAll(
    for {
      amountOfChannles       <- Gen.chooseNum[Int](10, 100)
      amountOfPeekedChannels <- Gen.chooseNum[Int](5, amountOfChannles)
    } yield (amountOfChannles, amountOfPeekedChannels),
    minSuccessful(100)
  ) {
    case (amountOfChannles: Int, amountOfPeekedChannels: Int) =>
      fixture { (store, replayStore, space, replaySpace) =>
        val channelsRange = List.range(0, amountOfChannles)
        val channels      = channelsRange.map(i => s"channel$i")
        val patterns      = channels.map(kp(Wildcard))
        val continuation  = "continuation"
        val peeks: SortedSet[Int] =
          SortedSet.apply(Random.shuffle(channelsRange).take(amountOfPeekedChannels): _*)
        val produces = Random.shuffle(channels)
        def consumeAndProduce(s: ISpace[Task, String, Pattern, String, String]) =
          for {
            r  <- s.consume(channels, patterns, continuation, false, peeks = peeks)
            rs <- produces.traverse(ch => s.produce(ch, s"datum-$ch", false))
          } yield rs

        for {
          emptyPoint <- space.createCheckpoint()
          rs         <- consumeAndProduce(space)
          _          = rs.flatten should have size 1
          hs         = store.get()
          _ <- channelsRange.traverse { i =>
                val ch = s"channel$i"
                hs.getData(ch).map { data =>
                  if (!peeks.contains(i))
                    data should have size 0
                }
              }
          rigPoint   <- space.createCheckpoint()
          _          <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)
          rrs        <- consumeAndProduce(replaySpace)
          finalPoint <- replaySpace.createCheckpoint()
          _          = rs should contain theSameElementsAs rrs
          _          = finalPoint.root shouldBe rigPoint.root
          _          = replaySpace.replayData shouldBe empty
        } yield ()
      }
  }

  "Picking n datums from m persistent waiting datums" should "replay correctly" in forAll(
    arbitraryRangesSize
  ) {
    case (n: Int, m: Int) =>
      fixture { (store, replayStore, space, replaySpace) =>
        for {
          emptyPoint <- space.createCheckpoint()

          range = (n until m).toList

          _ <- produceMany(
                space,
                range,
                channelCreator = kp("ch1"),
                datumCreator = i => s"datum$i",
                persist = true
              )
          results <- consumeMany(
                      space,
                      range,
                      channelsCreator = kp(List("ch1")),
                      patterns = List(Wildcard),
                      continuationCreator = i => s"continuation$i",
                      persist = false
                    )
          rigPoint <- space.createCheckpoint()

          _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

          _ <- produceMany(
                replaySpace,
                range,
                channelCreator = kp("ch1"),
                datumCreator = i => s"datum$i",
                persist = true
              )
          replayResults <- consumeMany(
                            replaySpace,
                            range,
                            channelsCreator = kp(List("ch1")),
                            patterns = List(Wildcard),
                            continuationCreator = i => s"continuation$i",
                            persist = false
                          )
          finalPoint <- replaySpace.createCheckpoint()

          _ = replayResults should contain theSameElementsAs results
          _ = finalPoint.root shouldBe rigPoint.root
          _ = replaySpace.replayData shouldBe empty
        } yield ()
      }
  }

  "Picking n continuations from m waiting continuations" should "replay correctly" in forAll(
    arbitraryRangesSize
  ) {
    case (n: Int, m: Int) =>
      fixture { (store, replayStore, space, replaySpace) =>
        for {
          emptyPoint <- space.createCheckpoint()

          range = (n until m).toList

          _ <- consumeMany(
                space,
                range,
                channelsCreator = kp(List("ch1")),
                patterns = List(Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = false
              )
          results <- produceMany(
                      space,
                      range,
                      channelCreator = kp("ch1"),
                      datumCreator = i => s"datum$i",
                      persist = false
                    )
          rigPoint <- space.createCheckpoint()

          _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

          _ <- consumeMany(
                replaySpace,
                range,
                channelsCreator = kp(List("ch1")),
                patterns = List(Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = false
              )
          replayResults <- produceMany(
                            replaySpace,
                            range,
                            channelCreator = kp("ch1"),
                            datumCreator = i => s"datum$i",
                            persist = false
                          )
          finalPoint <- replaySpace.createCheckpoint()

          _ = replayResults should contain theSameElementsAs results
          _ = finalPoint.root shouldBe rigPoint.root
          _ = replaySpace.replayData shouldBe empty
        } yield ()
      }
  }

  "Picking n continuations from m persistent waiting continuations" should "replay correctly" in forAll(
    arbitraryRangesSize
  ) {
    case (n: Int, m: Int) =>
      fixture { (store, replayStore, space, replaySpace) =>
        for {
          emptyPoint <- space.createCheckpoint()

          range = (n until m).toList

          _ <- consumeMany(
                space,
                range,
                channelsCreator = kp(List("ch1")),
                patterns = List(Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = true
              )
          results <- produceMany(
                      space,
                      range,
                      channelCreator = kp("ch1"),
                      datumCreator = i => s"datum$i",
                      persist = false
                    )
          rigPoint <- space.createCheckpoint()

          _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

          _ <- consumeMany(
                replaySpace,
                range,
                channelsCreator = kp(List("ch1")),
                patterns = List(Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = true
              )
          replayResults <- produceMany(
                            replaySpace,
                            range,
                            channelCreator = kp("ch1"),
                            datumCreator = i => s"datum$i",
                            persist = false
                          )
          finalPoint <- replaySpace.createCheckpoint()

          _ = replayResults should contain theSameElementsAs results
          _ = finalPoint.root shouldBe rigPoint.root
          _ = replaySpace.replayData shouldBe empty
        } yield ()
      }
  }

  "Pick n continuations from m waiting continuations stored at two channels" should "replay correctly" in forAll(
    arbitraryRangesSize
  ) {
    case (n: Int, m: Int) =>
      fixture { (store, replayStore, space, replaySpace) =>
        for {
          emptyPoint <- space.createCheckpoint()

          range = (n until m).toList

          _ <- consumeMany(
                space,
                range,
                channelsCreator = kp(List("ch1", "ch2")),
                patterns = List(Wildcard, Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = false
              )
          _ <- produceMany(
                space,
                range,
                channelCreator = kp("ch1"),
                datumCreator = i => s"datum$i",
                persist = false
              )
          results <- produceMany(
                      space,
                      range,
                      channelCreator = kp("ch2"),
                      datumCreator = i => s"datum$i",
                      persist = false
                    )
          rigPoint <- space.createCheckpoint()

          _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

          _ <- consumeMany(
                replaySpace,
                range,
                channelsCreator = kp(List("ch1", "ch2")),
                patterns = List(Wildcard, Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = false
              )
          _ <- produceMany(
                replaySpace,
                range,
                channelCreator = kp("ch1"),
                datumCreator = i => s"datum$i",
                persist = false
              )
          replayResults <- produceMany(
                            replaySpace,
                            range,
                            channelCreator = kp("ch2"),
                            datumCreator = i => s"datum$i",
                            persist = false
                          )
          finalPoint <- replaySpace.createCheckpoint()

          _ = replayResults should contain theSameElementsAs results
          _ = finalPoint.root shouldBe rigPoint.root
          _ = replaySpace.replayData shouldBe empty
        } yield ()
      }
  }

  "Picking n datums from m waiting datums while doing a bunch of other junk" should "replay correctly" in forAll(
    arbitraryRangesSize
  ) {
    case (n: Int, m: Int) =>
      fixture { (store, replayStore, space, replaySpace) =>
        for {
          emptyPoint <- space.createCheckpoint()

          _ <- produceMany(
                space,
                range = n until m toList,
                channelCreator = kp("ch1"),
                datumCreator = i => s"datum$i",
                persist = false
              )
          _ <- consumeMany(
                space,
                range = (m + 1 until m + 10).toList,
                channelsCreator = i => List(s"ch$i"),
                patterns = List(Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = false
              )
          _ <- produceMany(
                space,
                range = m + 11 until m + 20 toList,
                channelCreator = i => s"ch$i",
                datumCreator = i => s"datum$i",
                persist = false
              )
          results <- consumeMany(
                      space,
                      range = (n until m).toList,
                      channelsCreator = kp(List("ch1")),
                      patterns = List(Wildcard),
                      continuationCreator = i => s"continuation$i",
                      persist = false
                    )
          rigPoint <- space.createCheckpoint()

          _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

          _ <- produceMany(
                replaySpace,
                range = n until m toList,
                channelCreator = kp("ch1"),
                datumCreator = i => s"datum$i",
                persist = false
              )
          _ <- consumeMany(
                replaySpace,
                range = m + 1 until m + 10 toList,
                channelsCreator = i => List(s"ch$i"),
                patterns = List(Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = false
              )
          _ <- produceMany(
                replaySpace,
                range = m + 11 until m + 20 toList,
                channelCreator = i => s"ch$i",
                datumCreator = i => s"datum$i",
                persist = false
              )
          replayResults <- consumeMany(
                            replaySpace,
                            range = n until m toList,
                            channelsCreator = kp(List("ch1")),
                            patterns = List(Wildcard),
                            continuationCreator = i => s"continuation$i",
                            persist = false
                          )
          finalPoint <- replaySpace.createCheckpoint()

          _ = replayResults should contain theSameElementsAs results
          _ = finalPoint.root shouldBe rigPoint.root
          _ = replaySpace.replayData shouldBe empty
        } yield ()
      }
  }

  "Picking n continuations from m persistent waiting continuations while doing a bunch of other junk" should "replay correctly" in forAll(
    arbitraryRangesSize
  ) {
    case (n: Int, m: Int) =>
      fixture { (store, replayStore, space, replaySpace) =>
        for {
          emptyPoint <- space.createCheckpoint()

          _ <- consumeMany(
                space,
                range = n until m toList,
                channelsCreator = i => List(s"ch$i"),
                patterns = List(Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = false
              )
          _ <- produceMany(
                space,
                range = m + 1 until m + 10 toList,
                channelCreator = kp("ch1"),
                datumCreator = i => s"datum$i",
                persist = false
              )
          _ <- consumeMany(
                space,
                range = m + 11 until m + 20 toList,
                channelsCreator = kp(List("ch1")),
                patterns = List(Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = false
              )
          results <- produceMany(
                      space,
                      range = n until m,
                      channelCreator = i => s"ch$i",
                      datumCreator = i => s"datum$i",
                      persist = false
                    )
          rigPoint <- space.createCheckpoint()

          _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

          _ <- consumeMany(
                replaySpace,
                range = n until m toList,
                channelsCreator = i => List(s"ch$i"),
                patterns = List(Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = false
              )
          _ <- produceMany(
                replaySpace,
                range = m + 1 until m + 10 toList,
                channelCreator = kp("ch1"),
                datumCreator = i => s"datum$i",
                persist = false
              )
          _ <- consumeMany(
                replaySpace,
                range = m + 11 until m + 20 toList,
                channelsCreator = kp(List("ch1")),
                patterns = List(Wildcard),
                continuationCreator = i => s"continuation$i",
                persist = false
              )
          replayResults <- produceMany(
                            replaySpace,
                            range = n until m toList,
                            channelCreator = i => s"ch$i",
                            datumCreator = i => s"datum$i",
                            persist = false
                          )
          finalPoint <- replaySpace.createCheckpoint()

          _ = replayResults should contain theSameElementsAs results
          _ = finalPoint.root shouldBe rigPoint.root
          _ = replaySpace.replayData shouldBe empty
        } yield ()
      }
  }

  "Peeking data stored at two channels in 100 continuations" should "replay correctly" in
    fixture { (store, replayStore, space, replaySpace) =>
      for {
        emptyPoint <- space.createCheckpoint()

        range1 = 0 until 100
        range2 = 0 until 3
        range3 = 0 until 5
        _ <- produceMany(
              space,
              range2,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        _ <- produceMany(
              space,
              range3,
              channelCreator = kp("ch2"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        results <- consumeMany(
                    space,
                    range1,
                    channelsCreator = kp(List("ch1", "ch2")),
                    patterns = List(Wildcard, Wildcard),
                    continuationCreator = i => s"continuation$i",
                    persist = false,
                    peeks = SortedSet(0, 1)
                  )
        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)
        _ <- produceMany(
              replaySpace,
              range2,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        _ <- produceMany(
              replaySpace,
              range3,
              channelCreator = kp("ch2"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        replayResults <- consumeMany(
                          replaySpace,
                          range1,
                          channelsCreator = kp(List("ch1", "ch2")),
                          patterns = List(Wildcard, Wildcard),
                          continuationCreator = i => s"continuation$i",
                          persist = false,
                          peeks = SortedSet(0, 1)
                        )
        finalPoint <- replaySpace.createCheckpoint()

        _ = replayResults should contain theSameElementsAs results
        _ = finalPoint.root shouldBe rigPoint.root
        _ = replaySpace.replayData shouldBe empty
      } yield ()
    }

  "Replay rspace" should "correctly remove things from replay data" in fixture {
    (store, replayStore, space, replaySpace) =>
      val channels = List("ch1")
      val patterns = List[Pattern](Wildcard)
      val k        = "continuation"
      val datum    = "datum"
      for {
        emptyPoint <- space.createCheckpoint()

        cr = Consume(channels, patterns, k, persistent = false)

        _ <- consumeMany(
              space,
              range = 0 to 1 toList,
              channelsCreator = kp(channels),
              patterns = patterns,
              continuationCreator = kp(k),
              persist = false
            )
        _ <- produceMany(
              space,
              range = 0 to 1 toList,
              channelCreator = kp(channels(0)),
              datumCreator = kp(datum),
              persist = false
            )
        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

        _ = replaySpace.replayData.get(cr).map(_.size).value shouldBe 2

        _ <- replaySpace.consume(channels, patterns, k, persist = false)
        _ <- replaySpace.consume(channels, patterns, k, persist = false)
        _ <- replaySpace.produce(channels(0), datum, persist = false)

        _ = replaySpace.replayData.get(cr).map(_.size).value shouldBe 1

        _ <- replaySpace.produce(channels(0), datum, persist = false)
        _ = replaySpace.replayData.get(cr) shouldBe None
      } yield ()
  }

  "producing" should "return same, stable checkpoint root hashes" in forAll(arbitraryRangeSize) {
    n: Int =>
      def process(indices: Seq[Int]): Checkpoint = fixture {
        (store, replayStore, space, replaySpace) =>
          Task.delay {
            for (i <- indices) {
              replaySpace.produce("ch1", s"datum$i", false).unsafeRunSync
            }
            space.createCheckpoint().unsafeRunSync
          }
      }

      val cp1 = process(0 to n)
      val cp2 = process(n to 0 by -1)
      cp1.root shouldBe cp2.root
  }

  "an install" should "be available after resetting to a checkpoint" in fixture {
    (store, replayStore, space, replaySpace) =>
      val channel      = "ch1"
      val datum        = "datum1"
      val key          = List(channel)
      val patterns     = List(Wildcard)
      val continuation = "continuation"

      for {
        _ <- space.install(key, patterns, continuation)
        _ <- replaySpace.install(key, patterns, continuation)

        produce1     <- space.produce(channel, datum, persist = false)
        _            = produce1 shouldBe defined
        afterProduce <- space.createCheckpoint()

        _ <- replaySpace.rigAndReset(afterProduce.root, afterProduce.log)

        produce2 <- replaySpace.produce(channel, datum, persist = false)
        _        = produce2 shouldBe defined
      } yield ()
  }

  "reset" should
    """|empty the replay store,
       |reset the replay trie updates log,
       |and reset the replay data""".stripMargin in
    fixture { (_, replayStore, space, replaySpace) =>
      val channels     = List("ch1")
      val patterns     = List(Wildcard)
      val continuation = "continuation"

      for {
        emptyPoint <- space.createCheckpoint()

        consume1 <- space.consume(channels, patterns, continuation, false)
        _        = consume1 shouldBe None

        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

        consume2 <- replaySpace.consume(channels, patterns, continuation, false)
        _        = consume2 shouldBe None

        _ <- replayStore.get().isEmpty.map(_ shouldBe false)
        _ <- replayStore
              .get()
              .changes
              .map(collectActions[InsertContinuations[String, Pattern, String]])
              .map(_.length shouldBe 1)

        _ <- replaySpace.reset(emptyPoint.root)
        _ <- replayStore.get().isEmpty.map(_ shouldBe true)
        _ = replaySpace.replayData shouldBe empty

        checkpoint1 <- replaySpace.createCheckpoint()
        _           = checkpoint1.log shouldBe empty
      } yield ()
    }

  "clear" should
    """|empty the replay store,
       |reset the replay event log,
       |reset the replay trie updates log,
       |and reset the replay data""".stripMargin in
    fixture { (store, replayStore, space, replaySpace) =>
      val channels     = List("ch1")
      val patterns     = List(Wildcard)
      val continuation = "continuation"

      for {
        emptyPoint <- space.createCheckpoint()

        consume1 <- space.consume(channels, patterns, continuation, false)
        _        = consume1 shouldBe None

        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rigAndReset(emptyPoint.root, rigPoint.log)

        consume2 <- replaySpace.consume(channels, patterns, continuation, false)
        _        = consume2 shouldBe None

        _ <- replayStore.get().isEmpty.map(_ shouldBe false)
        _ <- replayStore
              .get()
              .changes
              .map(collectActions[InsertContinuations[String, Pattern, String]])
              .map(_.length shouldBe 1)

        checkpoint0 <- replaySpace.createCheckpoint()
        _           = checkpoint0.log shouldBe empty // we don't record trace logs in ReplayRspace

        _ <- replaySpace.clear()
        _ = replayStore.get().isEmpty.map(_ shouldBe true)
        _ = replaySpace.replayData shouldBe empty

        checkpoint1 <- replaySpace.createCheckpoint()
        _           = checkpoint1.log shouldBe empty
      } yield ()
    }

  "replay" should "not allow for ambiguous executions" in fixture {
    (store, replayStore, space, replaySpace) =>
      val noMatch                 = None
      val channel1                = "ch1"
      val channel2                = "ch2"
      val key1                    = List(channel1, channel2)
      val patterns: List[Pattern] = List(Wildcard, Wildcard)
      val continuation1           = "continuation1"
      val continuation2           = "continuation2"
      val data1                   = "datum1"
      val data2                   = "datum2"
      val data3                   = "datum3"

      implicit class AnyShouldF[F[_]: Functor, T](leftSideValue: F[T]) {
        def shouldBeF(value: T): F[Assertion] =
          leftSideValue.map(_ shouldBe value)

        def shouldNotBeF(value: T): F[Assertion] =
          leftSideValue.map(_ should not be value)
      }

      for {
        emptyCh <- space.createCheckpoint()
        _       <- space.produce(channel1, data3, false) shouldBeF noMatch
        _       <- space.produce(channel1, data3, false) shouldBeF noMatch
        _       <- space.produce(channel2, data1, false) shouldBeF noMatch

        _ <- space
              .consume(key1, patterns, continuation1, false) shouldNotBeF Option.empty
        //continuation1 produces data1 on ch2
        _ <- space.produce(channel2, data1, false) shouldBeF noMatch
        _ <- space
              .consume(key1, patterns, continuation2, false) shouldNotBeF Option.empty
        //continuation2 produces data2 on ch2
        _         <- space.produce(channel2, data2, false) shouldBeF noMatch
        afterPlay <- space.createCheckpoint()

        //rig
        _ <- replaySpace.rigAndReset(emptyCh.root, afterPlay.log)

        _ <- replaySpace.produce(channel1, data3, false) shouldBeF noMatch
        _ <- replaySpace.produce(channel1, data3, false) shouldBeF noMatch
        _ <- replaySpace.produce(channel2, data1, false) shouldBeF noMatch
        _ <- replaySpace.consume(key1, patterns, continuation2, false) shouldBeF noMatch

        _ <- replaySpace
              .consume(key1, patterns, continuation1, false) shouldNotBeF Option.empty
        //continuation1 produces data1 on ch2
        _ <- replaySpace
              .produce(channel2, data1, false) shouldNotBeF Option.empty //matches continuation2
        //continuation2 produces data2 on ch2
        _ <- replaySpace.produce(channel2, data2, false) shouldBeF noMatch

        _ = replaySpace.replayData.isEmpty shouldBe true
      } yield ()
  }

  "checkReplayData" should "proceed if replayData is empty" in fixture { (_, _, _, replaySpace) =>
    replaySpace.checkReplayData()
  }

  it should "raise an error if replayData contains elements" in fixture {
    (_, _, space, replaySpace) =>
      val channel      = "ch1"
      val channels     = List(channel)
      val patterns     = List(Wildcard)
      val datum        = "datum"
      val continuation = "continuation"

      for {
        _        <- space.consume(channels, patterns, continuation, false)
        _        <- space.produce(channel, datum, false)
        c        <- space.createCheckpoint()
        _        <- replaySpace.rigAndReset(c.root, c.log)
        res      <- replaySpace.checkReplayData().attempt
        Left(ex) = res
      } yield ex shouldBe a[ReplayException]
  }

}

trait ReplayRSpaceTestsBase[C, P, A, K]
    extends FlatSpec
    with Matchers
    with OptionValues
    with PropertyChecks {
  val logger = Logger(this.getClass.getName.stripSuffix("$"))

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSize = 10, minSuccessful = 5)

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  def fixture[S](
      f: (
          AtomicAny[HotStore[Task, C, P, A, K]],
          AtomicAny[HotStore[Task, C, P, A, K]],
          ISpace[Task, C, P, A, K],
          IReplaySpace[Task, C, P, A, K]
      ) => Task[S]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[Task, P, A]
  ): S
}

trait InMemoryReplayRSpaceTestsBase[C, P, A, K] extends ReplayRSpaceTestsBase[C, P, A, K] {
  import SchedulerPools.global
  override def fixture[S](
      f: (
          AtomicAny[HotStore[Task, C, P, A, K]],
          AtomicAny[HotStore[Task, C, P, A, K]],
          ISpace[Task, C, P, A, K],
          IReplaySpace[Task, C, P, A, K]
      ) => Task[S]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      m: Match[Task, P, A]
  ): S = {
    implicit val log: Log[Task]          = Log.log[Task]
    implicit val metricsF: Metrics[Task] = new Metrics.MetricsNOP[Task]()
    implicit val spanF: Span[Task]       = NoopSpan[Task]()
    implicit val kvm                     = InMemoryStoreManager[Task]

    (for {
      roots    <- kvm.store("roots")
      cold     <- kvm.store("cold")
      history  <- kvm.store("history")
      channels <- kvm.store("channels")
      historyRepository <- HistoryRepositoryInstances.lmdbRepository[Task, C, P, A, K](
                            roots,
                            cold,
                            history,
                            channels
                          )
      cache         <- Ref[Task].of(HotStoreState[C, P, A, K]())
      historyReader <- historyRepository.getHistoryReader(historyRepository.root)
      store <- {
        val hr = historyReader.base
        HotStore[Task, C, P, A, K](cache, hr).map(AtomicAny(_))
      }

      space = new RSpace[Task, C, P, A, K](
        historyRepository,
        store
      )
      historyCache <- Ref[Task].of(HotStoreState[C, P, A, K]())
      replayStore <- {
        val hr = historyReader.base
        HotStore[Task, C, P, A, K](historyCache, hr).map(AtomicAny(_))
      }
      replaySpace = new ReplayRSpace[Task, C, P, A, K](
        historyRepository,
        replayStore
      )
      res <- f(store, replayStore, space, replaySpace)
    } yield { res }).unsafeRunSync
  }
}

class InMemoryReplayRSpaceTests
    extends InMemoryReplayRSpaceTestsBase[String, Pattern, String, String]
    with ReplayRSpaceTests
