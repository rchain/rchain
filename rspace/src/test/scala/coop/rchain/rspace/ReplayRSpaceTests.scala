package coop.rchain.rspace

import java.nio.file.Files

import cats.Functor
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.catscontrib.ski._
import cats.effect._
import cats.implicits._
import com.typesafe.scalalogging.Logger
import coop.rchain.metrics.Metrics
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.{Branch, InMemoryTrieStore}
import coop.rchain.rspace.internal.GNAT
import coop.rchain.rspace.trace.Consume
import coop.rchain.shared.PathOps._
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest._

import scala.collection.immutable
import scala.util.{Random, Right}

object SchedulerPools {
  implicit val global = Scheduler.fixedPool("GlobalPool", 20)
  val rspacePool      = Scheduler.fixedPool("RSpacePool", 5)
}

//noinspection ZeroIndexToHead,NameBooleanParameters
trait ReplayRSpaceTests
    extends ReplayRSpaceTestsBase[String, Pattern, String, String]
    with TestImplicitHelpers {

  import SchedulerPools.global

  implicit val log: Log[Task] = new Log.NOPLog[Task]

  def consumeMany[C, P, A, R, K](
      space: ISpace[Task, C, P, A, R, K],
      range: Range,
      shuffle: Boolean,
      channelsCreator: Int => List[C],
      patterns: List[P],
      continuationCreator: Int => K,
      persist: Boolean
  )(
      implicit matcher: Match[Task, P, A, R]
  ): Task[List[Option[(ContResult[C, P, K], Seq[Result[R]])]]] =
    (if (shuffle) Random.shuffle(range.toList) else range.toList).parTraverse { i: Int =>
      logger.debug("Started consume {}", i)
      space
        .consume(channelsCreator(i), patterns, continuationCreator(i), persist)
        .map { r =>
          logger.debug("Finished consume {}", i)
          r
        }
    }

  def produceMany[C, P, A, R, K](
      space: ISpace[Task, C, P, A, R, K],
      range: Range,
      shuffle: Boolean,
      channelCreator: Int => C,
      datumCreator: Int => A,
      persist: Boolean
  )(
      implicit matcher: Match[Task, P, A, R]
  ): Task[List[Option[(ContResult[C, P, K], immutable.Seq[Result[R]])]]] =
    (if (shuffle) Random.shuffle(range.toList) else range.toList).parTraverse { i: Int =>
      logger.debug("Started produce {}", i)
      space.produce(channelCreator(i), datumCreator(i), persist).map { r =>
        logger.debug("Finished produce {}", i)
        r
      }
    }

  "reset to a checkpoint from a different branch" should "work" in withTestSpaces {
    (space, replaySpace) =>
      for {
        root0 <- replaySpace.createCheckpoint().map(_.root)
        _     = replaySpace.store.isEmpty shouldBe true

        _     <- space.produce("ch1", "datum1", false)
        root1 <- space.createCheckpoint().map(_.root)

        _ <- replaySpace.reset(root1)
        _ = replaySpace.store.isEmpty shouldBe false

        _ <- space.reset(root0)
        _ = space.store.isEmpty shouldBe true
      } yield ()
  }

  "Creating a COMM Event" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      val channels     = List("ch1")
      val patterns     = List(Wildcard)
      val continuation = "continuation"
      val datum        = "datum1"

      for {
        emptyPoint <- space.createCheckpoint()

        resultConsume <- space.consume(channels, patterns, continuation, false)
        resultProduce <- space.produce(channels(0), datum, false)
        rigPont       <- space.createCheckpoint()

        _ = resultConsume shouldBe None
        _ = resultProduce shouldBe defined

        _ <- replaySpace.rig(emptyPoint.root, rigPont.log)

        replayResultConsume <- replaySpace.consume(channels, patterns, continuation, false)
        replayResultProduce <- replaySpace.produce(channels(0), datum, false)
        finalPoint          <- space.createCheckpoint()

        _ = replayResultConsume shouldBe None
        _ = replayResultProduce shouldBe resultProduce
        _ = finalPoint.root shouldBe rigPont.root
        _ = replaySpace.replayData shouldBe empty
      } yield ()
    }

  "Picking a datum from 100 waiting datums" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      for {
        emptyPoint <- space.createCheckpoint()

        range = 0 until 100
        _ <- produceMany(
              space,
              range,
              shuffle = false,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        result <- space.consume(
                   channels = List("ch1"),
                   patterns = List(Wildcard),
                   continuation = "continuation1",
                   persist = false
                 )
        rigPoint <- space.createCheckpoint()
        _        <- replaySpace.rig(emptyPoint.root, rigPoint.log)
        _ <- produceMany(
              replaySpace,
              range,
              shuffle = true,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        replayResult <- replaySpace.consume(
                         channels = List("ch1"),
                         patterns = List(Wildcard),
                         continuation = "continuation1",
                         persist = false
                       )
        finalPoint <- replaySpace.createCheckpoint()
        _          = replayResult shouldBe result
        _          = finalPoint.root shouldBe rigPoint.root
        _          = replaySpace.replayData shouldBe empty
      } yield ()
    }

  "Picking 100 datums from 100 waiting datums" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      for {
        emptyPoint <- space.createCheckpoint()

        range = 0 until 100

        _ <- produceMany(
              space,
              range,
              shuffle = false,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        results <- consumeMany(
                    space,
                    range,
                    shuffle = false,
                    channelsCreator = kp(List("ch1")),
                    patterns = List(Wildcard),
                    continuationCreator = i => s"continuation$i",
                    persist = false
                  )
        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rig(emptyPoint.root, rigPoint.log)

        _ <- produceMany(
              replaySpace,
              range,
              shuffle = true,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        replayResults <- consumeMany(
                          replaySpace,
                          range,
                          shuffle = true,
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

  "Picking 100 datums from 100 persistent waiting datums" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      for {
        emptyPoint <- space.createCheckpoint()

        range = 0 until 100

        _ <- produceMany(
              space,
              range,
              shuffle = false,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = true
            )
        results <- consumeMany(
                    space,
                    range,
                    shuffle = false,
                    channelsCreator = kp(List("ch1")),
                    patterns = List(Wildcard),
                    continuationCreator = i => s"continuation$i",
                    persist = false
                  )
        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rig(emptyPoint.root, rigPoint.log)

        _ <- produceMany(
              replaySpace,
              range,
              shuffle = true,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = true
            )
        replayResults <- consumeMany(
                          replaySpace,
                          range,
                          shuffle = true,
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

  "Picking a continuation from 100 waiting continuations" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      for {
        emptyPoint <- space.createCheckpoint()
        range      = 0 until 100
        _ <- consumeMany(
              space,
              range,
              shuffle = false,
              channelsCreator = kp(List("ch1")),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        result <- space.produce(
                   channel = "ch1",
                   data = "datum1",
                   persist = false
                 )
        rigPoint <- space.createCheckpoint()
        _        <- replaySpace.rig(emptyPoint.root, rigPoint.log)
        _ <- consumeMany(
              replaySpace,
              range,
              shuffle = true,
              channelsCreator = kp(List("ch1")),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        replayResult <- replaySpace.produce(
                         channel = "ch1",
                         data = "datum1",
                         persist = false
                       )
        finalPoint <- replaySpace.createCheckpoint()

        _ = replayResult shouldBe result
        _ = finalPoint.root shouldBe rigPoint.root
        _ = replaySpace.replayData shouldBe empty
      } yield ()
    }

  "Picking 100 continuations from 100 waiting continuations" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      for {
        emptyPoint <- space.createCheckpoint()

        range = 0 until 100

        _ <- consumeMany(
              space,
              range,
              shuffle = false,
              channelsCreator = kp(List("ch1")),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        results <- produceMany(
                    space,
                    range,
                    shuffle = false,
                    channelCreator = kp("ch1"),
                    datumCreator = i => s"datum$i",
                    persist = false
                  )
        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rig(emptyPoint.root, rigPoint.log)

        _ <- consumeMany(
              replaySpace,
              range,
              shuffle = true,
              channelsCreator = kp(List("ch1")),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        replayResults <- produceMany(
                          replaySpace,
                          range,
                          shuffle = true,
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

  "Picking 100 continuations from 100 persistent waiting continuations" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      for {
        emptyPoint <- space.createCheckpoint()

        range = 0 until 100

        _ <- consumeMany(
              space,
              range,
              shuffle = false,
              channelsCreator = kp(List("ch1")),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = true
            )
        results <- produceMany(
                    space,
                    range,
                    shuffle = false,
                    channelCreator = kp("ch1"),
                    datumCreator = i => s"datum$i",
                    persist = false
                  )
        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rig(emptyPoint.root, rigPoint.log)

        _ <- consumeMany(
              replaySpace,
              range,
              shuffle = true,
              channelsCreator = kp(List("ch1")),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = true
            )
        replayResults <- produceMany(
                          replaySpace,
                          range,
                          shuffle = true,
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

  "Pick 100 continuations from 100 waiting continuations stored at two channels" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      for {
        emptyPoint <- space.createCheckpoint()

        range = 0 until 100

        _ <- consumeMany(
              space,
              range,
              shuffle = false,
              channelsCreator = kp(List("ch1", "ch2")),
              patterns = List(Wildcard, Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        _ <- produceMany(
              space,
              range,
              shuffle = false,
              channelCreator = kp("ch1"),
              datumCreator = kp("datum1"),
              persist = false
            )
        results <- produceMany(
                    space,
                    range,
                    shuffle = false,
                    channelCreator = kp("ch2"),
                    datumCreator = kp("datum2"),
                    persist = false
                  )
        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rig(emptyPoint.root, rigPoint.log)

        _ <- consumeMany(
              replaySpace,
              range,
              shuffle = true,
              channelsCreator = kp(List("ch1", "ch2")),
              patterns = List(Wildcard, Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        _ <- produceMany(
              replaySpace,
              range,
              shuffle = true,
              channelCreator = kp("ch1"),
              datumCreator = kp("datum1"),
              persist = false
            )
        replayResults <- produceMany(
                          replaySpace,
                          range,
                          shuffle = true,
                          channelCreator = kp("ch2"),
                          datumCreator = kp("datum2"),
                          persist = false
                        )
        finalPoint <- replaySpace.createCheckpoint()

        _ = replayResults should contain theSameElementsAs results
        _ = finalPoint.root shouldBe rigPoint.root
        _ = replaySpace.replayData shouldBe empty
      } yield ()
    }

  "Picking 100 datums from 100 waiting datums while doing a bunch of other junk" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      for {
        emptyPoint <- space.createCheckpoint()

        _ <- produceMany(
              space,
              range = 0 until 100,
              shuffle = false,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        _ <- consumeMany(
              space,
              range = 100 until 200,
              shuffle = false,
              channelsCreator = i => List(s"ch$i"),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        _ <- produceMany(
              space,
              range = 200 until 300,
              shuffle = false,
              channelCreator = i => s"ch$i",
              datumCreator = i => s"datum$i",
              persist = false
            )
        results <- consumeMany(
                    space,
                    range = 0 until 100,
                    shuffle = false,
                    channelsCreator = kp(List("ch1")),
                    patterns = List(Wildcard),
                    continuationCreator = i => s"continuation$i",
                    persist = false
                  )
        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rig(emptyPoint.root, rigPoint.log)

        _ <- produceMany(
              replaySpace,
              range = 0 until 100,
              shuffle = true,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        _ <- consumeMany(
              replaySpace,
              range = 100 until 200,
              shuffle = true,
              channelsCreator = i => List(s"ch$i"),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        _ <- produceMany(
              replaySpace,
              range = 200 until 300,
              shuffle = true,
              channelCreator = i => s"ch$i",
              datumCreator = i => s"datum$i",
              persist = false
            )
        replayResults <- consumeMany(
                          replaySpace,
                          range = 0 until 100,
                          shuffle = true,
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

  "Picking 100 continuations from 100 persistent waiting continuations while doing a bunch of other junk" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      for {
        emptyPoint <- space.createCheckpoint()

        _ <- consumeMany(
              space,
              range = 0 until 100,
              shuffle = false,
              channelsCreator = i => List(s"ch$i"),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        _ <- produceMany(
              space,
              range = 100 until 200,
              shuffle = false,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        _ <- consumeMany(
              space,
              range = 200 until 300,
              shuffle = false,
              channelsCreator = kp(List("ch1")),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        results <- produceMany(
                    space,
                    range = 0 until 100,
                    shuffle = false,
                    channelCreator = i => s"ch$i",
                    datumCreator = i => s"datum$i",
                    persist = false
                  )
        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rig(emptyPoint.root, rigPoint.log)

        _ <- consumeMany(
              replaySpace,
              range = 0 until 100,
              shuffle = true,
              channelsCreator = i => List(s"ch$i"),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        _ <- produceMany(
              replaySpace,
              range = 100 until 200,
              shuffle = true,
              channelCreator = kp("ch1"),
              datumCreator = i => s"datum$i",
              persist = false
            )
        _ <- consumeMany(
              replaySpace,
              range = 200 until 300,
              shuffle = true,
              channelsCreator = kp(List("ch1")),
              patterns = List(Wildcard),
              continuationCreator = i => s"continuation$i",
              persist = false
            )
        replayResults <- produceMany(
                          replaySpace,
                          range = 0 until 100,
                          shuffle = true,
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

  "Replay rspace" should "correctly remove things from replay data" in withTestSpaces {
    (space, replaySpace) =>
      val channels = List("ch1")
      val patterns = List[Pattern](Wildcard)
      val k        = "continuation"
      val datum    = "datum"
      for {
        emptyPoint <- space.createCheckpoint()

        cr = Consume.create(channels, patterns, k, persist = false)

        _ <- consumeMany(
              space,
              range = 0 to 1,
              shuffle = false,
              channelsCreator = kp(channels),
              patterns = patterns,
              continuationCreator = kp(k),
              persist = false
            )
        _ <- produceMany(
              space,
              range = 0 to 1,
              shuffle = false,
              channelCreator = kp(channels(0)),
              datumCreator = kp(datum),
              persist = false
            )
        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rig(emptyPoint.root, rigPoint.log)

        _ = replaySpace.replayData.get(cr).map(_.size).value shouldBe 2

        _ <- replaySpace.consume(channels, patterns, k, persist = false)
        _ <- replaySpace.consume(channels, patterns, k, persist = false)
        _ <- replaySpace.produce(channels(0), datum, persist = false)

        _ = replaySpace.replayData.get(cr).map(_.size).value shouldBe 1

        _ <- replaySpace.produce(channels(0), datum, persist = false)
        _ = replaySpace.replayData.get(cr) shouldBe None
      } yield ()
  }

  "producing" should "return same, stable checkpoint root hashes" in {
    def process(indices: Seq[Int]): Checkpoint = withTestSpaces { (space, replaySpace) =>
      Task.delay {
        for (i <- indices) {
          replaySpace.produce("ch1", s"datum$i", false).unsafeRunSync
        }
        space.createCheckpoint().unsafeRunSync
      }
    }

    val cp1 = process(0 to 10)
    val cp2 = process(10 to 0 by -1)
    cp1.root shouldBe cp2.root
  }

  "an install" should "be available after resetting to a checkpoint" in withTestSpaces {
    (space, replaySpace) =>
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

        _ <- replaySpace.rig(afterProduce.root, afterProduce.log)

        produce2 <- replaySpace.produce(channel, datum, persist = false)
        _        = produce2 shouldBe defined
      } yield ()
  }

  "reset" should
    """|empty the replay store,
       |reset the replay trie updates log,
       |and reset the replay data""".stripMargin in
    withTestSpaces { (space, replaySpace) =>
      val channels     = List("ch1")
      val patterns     = List(Wildcard)
      val continuation = "continuation"

      for {
        emptyPoint <- space.createCheckpoint()

        consume1 <- space.consume(channels, patterns, continuation, false)
        _        = consume1 shouldBe None

        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rig(emptyPoint.root, rigPoint.log)

        consume2 <- replaySpace.consume(channels, patterns, continuation, false)
        _        = consume2 shouldBe None

        replayStore = replaySpace.store

        _ = replayStore.isEmpty shouldBe false
        _ = replayStore.getTrieUpdates.length shouldBe 1
        _ = replayStore.getTrieUpdateCount shouldBe 1

        _ <- replaySpace.reset(emptyPoint.root)
        _ = replayStore.isEmpty shouldBe true
        _ = replayStore.getTrieUpdates.length shouldBe 0
        _ = replayStore.getTrieUpdateCount shouldBe 0
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
    withTestSpaces { (space, replaySpace) =>
      val channels     = List("ch1")
      val patterns     = List(Wildcard)
      val continuation = "continuation"

      for {
        emptyPoint <- space.createCheckpoint()

        consume1 <- space.consume(channels, patterns, continuation, false)
        _        = consume1 shouldBe None

        rigPoint <- space.createCheckpoint()

        _ <- replaySpace.rig(emptyPoint.root, rigPoint.log)

        consume2 <- replaySpace.consume(channels, patterns, continuation, false)
        _        = consume2 shouldBe None

        replayStore = replaySpace.store

        _ = replayStore.isEmpty shouldBe false
        _ = replayStore.getTrieUpdates.length shouldBe 1
        _ = replayStore.getTrieUpdateCount shouldBe 1

        checkpoint0 <- replaySpace.createCheckpoint()
        _           = checkpoint0.log shouldBe empty // we don't record trace logs in ReplayRspace

        _ <- replaySpace.clear()
        _ = replayStore.isEmpty shouldBe true
        _ = replayStore.getTrieUpdates.length shouldBe 0
        _ = replayStore.getTrieUpdateCount shouldBe 0
        _ = replaySpace.replayData shouldBe empty

        checkpoint1 <- replaySpace.createCheckpoint()
        _           = checkpoint1.log shouldBe empty
      } yield ()
    }

  "after close rspace" should "throw RSpaceClosedException on all store operations" in
    withTestSpaces { (space, replaySpace) =>
      val channel      = "ch1"
      val key          = List(channel)
      val patterns     = List(Wildcard)
      val continuation = "continuation"
      val data         = "datum1"

      Task.delay {
        replaySpace.close().unsafeRunSync

        an[RSpaceClosedException] shouldBe thrownBy(
          replaySpace.install(key, patterns, continuation).unsafeRunSync
        )

        an[RSpaceClosedException] shouldBe thrownBy(
          replaySpace.consume(key, patterns, continuation, false).unsafeRunSync
        )

        an[RSpaceClosedException] shouldBe thrownBy(
          replaySpace.produce(channel, data, false).unsafeRunSync
        )
      }
    }

  "replay" should "not allow for ambiguous executions" in withTestSpaces { (space, replaySpace) =>
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
      //some maliciously 'random' play order
      _ <- space.produce(channel1, data3, false, 0) shouldBeF noMatch
      _ <- space.produce(channel1, data3, false, 0) shouldBeF noMatch
      _ <- space.produce(channel2, data1, false, 0) shouldBeF noMatch

      _ <- space
            .consume(key1, patterns, continuation1, false, 0) shouldNotBeF Option.empty
      //continuation1 produces data1 on ch2
      _ <- space.produce(channel2, data1, false, 1) shouldBeF noMatch
      _ <- space
            .consume(key1, patterns, continuation2, false, 0) shouldNotBeF Option.empty
      //continuation2 produces data2 on ch2
      _         <- space.produce(channel2, data2, false, 2) shouldBeF noMatch
      afterPlay <- space.createCheckpoint()

      //rig
      _ <- replaySpace.rig(emptyCh.root, afterPlay.log)

      //some maliciously 'random' replay order
      _ <- replaySpace.produce(channel1, data3, false, 0) shouldBeF noMatch
      _ <- replaySpace.produce(channel1, data3, false, 0) shouldBeF noMatch
      _ <- replaySpace.produce(channel2, data1, false, 0) shouldBeF noMatch
      _ <- replaySpace.consume(key1, patterns, continuation2, false, 0) shouldBeF noMatch

      _ <- replaySpace
            .consume(key1, patterns, continuation1, false, 0) shouldNotBeF Option.empty
      //continuation1 produces data1 on ch2
      _ <- replaySpace
            .produce(channel2, data1, false, 1) shouldNotBeF Option.empty //matches continuation2
      //continuation2 produces data2 on ch2
      _ <- replaySpace.produce(channel2, data2, false, 1) shouldBeF noMatch

      _ = replaySpace.replayData.isEmpty shouldBe true
    } yield ()
  }
}

trait ReplayRSpaceTestsBase[C, P, A, K] extends FlatSpec with Matchers with OptionValues {
  val logger = Logger(this.getClass.getName.stripSuffix("$"))

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  def withTestSpaces[S](
      f: (ISpace[Task, C, P, A, A, K], IReplaySpace[Task, C, P, A, A, K]) => Task[S]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      oC: Ordering[C]
  ): S
}

trait LMDBReplayRSpaceTestsBase[C, P, E, A, K] extends ReplayRSpaceTestsBase[C, P, A, K] {
  import SchedulerPools.global
  override def withTestSpaces[S](
      f: (ISpace[Task, C, P, A, A, K], IReplaySpace[Task, C, P, A, A, K]) => Task[S]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      oC: Ordering[C]
  ): S = {
    implicit val log: Log[Task]          = Log.log[Task]
    implicit val metricsF: Metrics[Task] = new Metrics.MetricsNOP[Task]()
    val dedicated                        = SchedulerPools.rspacePool

    val dbDir   = Files.createTempDirectory("rchain-storage-test-")
    val context = Context.create[Task, C, P, A, K](dbDir, 1024L * 1024L * 4096L)
    val space = RSpace
      .create[Task, C, P, A, A, K](context, Branch.MASTER)(
        sc,
        sp,
        sa,
        sk,
        Concurrent[Task],
        log,
        ContextShift[Task],
        dedicated,
        metricsF
      )
      .unsafeRunSync
    val replaySpace =
      ReplayRSpace
        .create[Task, C, P, A, A, K](context, Branch.REPLAY)(
          sc,
          sp,
          sa,
          sk,
          Concurrent[Task],
          log,
          ContextShift[Task],
          dedicated,
          metricsF
        )
        .unsafeRunSync

    try {
      f(space, replaySpace).unsafeRunSync
    } finally {
      space.close()
      replaySpace.close()
      context.close()
      dbDir.recursivelyDelete()
    }
  }
}

trait MixedReplayRSpaceTestsBase[C, P, E, A, K] extends ReplayRSpaceTestsBase[C, P, A, K] {
  import SchedulerPools.global
  override def withTestSpaces[S](
      f: (ISpace[Task, C, P, A, A, K], IReplaySpace[Task, C, P, A, A, K]) => Task[S]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      oC: Ordering[C]
  ): S = {
    implicit val log: Log[Task]          = Log.log[Task]
    implicit val metricsF: Metrics[Task] = new Metrics.MetricsNOP[Task]()

    val dbDir   = Files.createTempDirectory("rchain-storage-test-")
    val context = Context.createMixed[Task, C, P, A, K](dbDir, 1024L * 1024L * 4096L)
    val space   = RSpace.create[Task, C, P, A, A, K](context, Branch.MASTER).unsafeRunSync
    val replaySpace =
      ReplayRSpace.create[Task, C, P, A, A, K](context, Branch.REPLAY).unsafeRunSync

    try {
      f(space, replaySpace).unsafeRunSync
    } finally {
      space.close()
      replaySpace.close()
      context.close()
      dbDir.recursivelyDelete()
    }
  }
}

trait InMemoryReplayRSpaceTestsBase[C, P, A, K] extends ReplayRSpaceTestsBase[C, P, A, K] {
  import SchedulerPools.global
  override def withTestSpaces[S](
      f: (ISpace[Task, C, P, A, A, K], IReplaySpace[Task, C, P, A, A, K]) => Task[S]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      oC: Ordering[C]
  ): S = {
    implicit val log: Log[Task]          = Log.log[Task]
    implicit val metricsF: Metrics[Task] = new Metrics.MetricsNOP[Task]()

    val ctx: Context[Task, C, P, A, K] = Context.createInMemory()
    val space                          = RSpace.create[Task, C, P, A, A, K](ctx, Branch.REPLAY).unsafeRunSync
    val replaySpace                    = ReplayRSpace.create[Task, C, P, A, A, K](ctx, Branch.REPLAY).unsafeRunSync

    try {
      f(space, replaySpace).unsafeRunSync
    } finally {
      space.close()
      replaySpace.close()
    }
  }
}

trait FaultyStoreReplayRSpaceTestsBase[C, P, A, K] extends ReplayRSpaceTestsBase[C, P, A, K] {
  import SchedulerPools.global
  override def withTestSpaces[S](
      f: (ISpace[Task, C, P, A, A, K], IReplaySpace[Task, C, P, A, A, K]) => Task[S]
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      oC: Ordering[C]
  ): S = {
    implicit val log: Log[Task]          = Log.log[Task]
    implicit val metricsF: Metrics[Task] = new Metrics.MetricsNOP[Task]()

    val trieStore = InMemoryTrieStore.create[Blake2b256Hash, GNAT[C, P, A, K]]()
    val mainStore = InMemoryStore
      .create[Task, InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]], C, P, A, K](
        trieStore,
        Branch.REPLAY
      )
    val space = RSpace.create[Task, C, P, A, A, K](mainStore, Branch.REPLAY).unsafeRunSync
    val store =
      new InMemoryStore[Task, InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]], C, P, A, K](
        trieStore,
        Branch.REPLAY
      ) {
        override private[rspace] def createTxnWrite()
          : InMemTransaction[coop.rchain.rspace.State[C, P, A, K]] =
          throw new RuntimeException("Couldn't write to underlying store")
      }

    val replaySpace = new ReplayRSpace[Task, C, P, A, A, K](store, Branch.REPLAY)

    try {
      f(space, replaySpace).unsafeRunSync
    } finally {
      space.close()
      replaySpace.close()
    }
  }

}

class LMDBReplayRSpaceTests
    extends LMDBReplayRSpaceTestsBase[String, Pattern, Nothing, String, String]
    with ReplayRSpaceTests {}

class InMemoryReplayRSpaceTests
    extends InMemoryReplayRSpaceTestsBase[String, Pattern, String, String]
    with ReplayRSpaceTests {}

class FaultyReplayRSpaceTests
    extends FaultyStoreReplayRSpaceTestsBase[String, Pattern, String, String] {
  import SchedulerPools.global

  "an exception thrown inside a consume" should "not make replay rspace unresponsive" in
    withTestSpaces { (space, replaySpace) =>
      val channel      = "ch1"
      val key          = List(channel)
      val patterns     = List(Wildcard)
      val continuation = "continuation"

      Task.delay {
        the[RuntimeException] thrownBy replaySpace
          .consume(
            key,
            patterns,
            continuation,
            false
          )
          .unsafeRunSync should have message "Couldn't write to underlying store"
      }
    }

  "an exception thrown inside a produce" should "not make replay rspace unresponsive" in
    withTestSpaces { (space, replaySpace) =>
      val channel = "ch1"
      val data    = "datum1"

      Task.delay {
        the[RuntimeException] thrownBy replaySpace
          .produce(
            channel,
            data,
            false
          )
          .unsafeRunSync should have message "Couldn't write to underlying store"
      }
    }
}
