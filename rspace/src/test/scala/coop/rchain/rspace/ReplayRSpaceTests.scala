package coop.rchain.rspace

import java.nio.file.Files

import cats.Id
import cats.effect._
import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import coop.rchain.rspace.ISpace.IdISpace
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.{Branch, InMemoryTrieStore}
import coop.rchain.rspace.internal.GNAT
import coop.rchain.rspace.spaces._
import coop.rchain.rspace.trace.{COMM, Consume, IOEvent, Produce}
import coop.rchain.shared.PathOps._
import org.scalatest._

import scala.Function.const
import scala.collection.parallel.ParSeq
import scala.collection.{immutable, mutable}
import scala.util.{Random, Right}
import scala.concurrent.ExecutionContext.Implicits.global

//noinspection ZeroIndexToHead,NameBooleanParameters
trait ReplayRSpaceTests
    extends ReplayRSpaceTestsBase[String, Pattern, Nothing, String, String]
    with TestImplicitHelpers {

  def consumeMany[C, P, A, R, K](
      space: IdISpace[C, P, Nothing, A, R, K],
      range: Range,
      shuffle: Boolean,
      channelsCreator: Int => List[C],
      patterns: List[P],
      continuationCreator: Int => K,
      persist: Boolean
  )(
      implicit matcher: Match[P, Nothing, A, R]
  ): ParSeq[Option[(ContResult[C, P, K], Seq[Result[R]])]] =
    (if (shuffle) Random.shuffle(range.toList) else range.toList).par.map { i: Int =>
      logger.debug("Started consume {}", i)
      val res =
        space.consume(channelsCreator(i), patterns, continuationCreator(i), persist).right.get
      logger.debug("Finished consume {}", i)
      res
    }

  def produceMany[C, P, A, R, K](
      space: IdISpace[C, P, Nothing, A, R, K],
      range: Range,
      shuffle: Boolean,
      channelCreator: Int => C,
      datumCreator: Int => A,
      persist: Boolean
  )(
      implicit matcher: Match[P, Nothing, A, R]
  ): ParSeq[Option[(ContResult[C, P, K], immutable.Seq[Result[R]])]] =
    (if (shuffle) Random.shuffle(range.toList) else range.toList).par.map { i: Int =>
      logger.debug("Started produce {}", i)
      val res = space.produce(channelCreator(i), datumCreator(i), persist).right.get
      logger.debug("Finished produce {}", i)
      res
    }

  "reset to a checkpoint from a different branch" should "work" in withTestSpaces {
    (space, replaySpace) =>
      val root0 = replaySpace.createCheckpoint().root
      replaySpace.store.isEmpty shouldBe true

      space.produce("ch1", "datum1", false)
      val root1 = space.createCheckpoint().root

      replaySpace.reset(root1)
      replaySpace.store.isEmpty shouldBe false

      space.reset(root0)
      space.store.isEmpty shouldBe true
  }

  "Creating a COMM Event" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      val channels     = List("ch1")
      val patterns     = List(Wildcard)
      val continuation = "continuation"
      val datum        = "datum1"

      val emptyPoint = space.createCheckpoint()

      val resultConsume = space.consume(channels, patterns, continuation, false)
      val resultProduce = space.produce(channels(0), datum, false)
      val rigPont       = space.createCheckpoint()

      resultConsume shouldBe Right(None)
      resultProduce shouldBe defined

      replaySpace.rig(emptyPoint.root, rigPont.log)

      val replayResultConsume = replaySpace.consume(channels, patterns, continuation, false)
      val replayResultProduce = replaySpace.produce(channels(0), datum, false)
      val finalPoint          = space.createCheckpoint()

      replayResultConsume shouldBe Right(None)
      replayResultProduce shouldBe resultProduce
      finalPoint.root shouldBe rigPont.root
      replaySpace.replayData shouldBe empty
    }

  "Picking a datum from 100 waiting datums" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      val emptyPoint = space.createCheckpoint()

      val range: Range = 0 until 100

      produceMany(
        space,
        range,
        shuffle = false,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      val result = space.consume(
        channels = List("ch1"),
        patterns = List(Wildcard),
        continuation = "continuation1",
        persist = false
      )
      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      produceMany(
        replaySpace,
        range,
        shuffle = true,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      val replayResult = replaySpace.consume(
        channels = List("ch1"),
        patterns = List(Wildcard),
        continuation = "continuation1",
        persist = false
      )
      val finalPoint = replaySpace.createCheckpoint()

      replayResult shouldBe result
      finalPoint.root shouldBe rigPoint.root
      replaySpace.replayData shouldBe empty
    }

  "Picking 100 datums from 100 waiting datums" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      val emptyPoint = space.createCheckpoint()

      val range: Range = 0 until 100

      produceMany(
        space,
        range,
        shuffle = false,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      val results = consumeMany(
        space,
        range,
        shuffle = false,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      produceMany(
        replaySpace,
        range,
        shuffle = true,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      val replayResults = consumeMany(
        replaySpace,
        range,
        shuffle = true,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val finalPoint = replaySpace.createCheckpoint()

      replayResults should contain theSameElementsAs results
      finalPoint.root shouldBe rigPoint.root
      replaySpace.replayData shouldBe empty
    }

  "Picking 100 datums from 100 persistent waiting datums" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      val emptyPoint = space.createCheckpoint()

      val range: Range = 0 until 100

      produceMany(
        space,
        range,
        shuffle = false,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = true
      )
      val results = consumeMany(
        space,
        range,
        shuffle = false,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      produceMany(
        replaySpace,
        range,
        shuffle = true,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = true
      )
      val replayResults = consumeMany(
        replaySpace,
        range,
        shuffle = true,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val finalPoint = replaySpace.createCheckpoint()

      replayResults should contain theSameElementsAs results
      finalPoint.root shouldBe rigPoint.root
      replaySpace.replayData shouldBe empty
    }

  "Picking a continuation from 100 waiting continuations" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      val emptyPoint = space.createCheckpoint()

      val range: Range = 0 until 100

      consumeMany(
        space,
        range,
        shuffle = false,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val result = space.produce(
        channel = "ch1",
        data = "datum1",
        persist = false
      )
      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      consumeMany(
        replaySpace,
        range,
        shuffle = true,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val replayResult = replaySpace.produce(
        channel = "ch1",
        data = "datum1",
        persist = false
      )
      val finalPoint = replaySpace.createCheckpoint()

      replayResult shouldBe result
      finalPoint.root shouldBe rigPoint.root
      replaySpace.replayData shouldBe empty
    }

  "Picking 100 continuations from 100 waiting continuations" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      val emptyPoint = space.createCheckpoint()

      val range: Range = 0 until 100

      consumeMany(
        space,
        range,
        shuffle = false,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val results = produceMany(
        space,
        range,
        shuffle = false,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      consumeMany(
        replaySpace,
        range,
        shuffle = true,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val replayResults = produceMany(
        replaySpace,
        range,
        shuffle = true,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      val finalPoint = replaySpace.createCheckpoint()

      replayResults should contain theSameElementsAs results
      finalPoint.root shouldBe rigPoint.root
      replaySpace.replayData shouldBe empty
    }

  "Picking 100 continuations from 100 persistent waiting continuations" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      val emptyPoint = space.createCheckpoint()

      val range: Range = 0 until 100

      consumeMany(
        space,
        range,
        shuffle = false,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = true
      )
      val results = produceMany(
        space,
        range,
        shuffle = false,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      consumeMany(
        replaySpace,
        range,
        shuffle = true,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = true
      )
      val replayResults = produceMany(
        replaySpace,
        range,
        shuffle = true,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      val finalPoint = replaySpace.createCheckpoint()

      replayResults should contain theSameElementsAs results
      finalPoint.root shouldBe rigPoint.root
      replaySpace.replayData shouldBe empty
    }

  "Pick 100 continuations from 100 waiting continuations stored at two channels" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      val emptyPoint = space.createCheckpoint()

      val range: Range = 0 until 100

      consumeMany(
        space,
        range,
        shuffle = false,
        channelsCreator = const(List("ch1", "ch2")),
        patterns = List(Wildcard, Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      produceMany(
        space,
        range,
        shuffle = false,
        channelCreator = const("ch1"),
        datumCreator = const("datum1"),
        persist = false
      )
      val results = produceMany(
        space,
        range,
        shuffle = false,
        channelCreator = const("ch2"),
        datumCreator = const("datum2"),
        persist = false
      )
      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      consumeMany(
        replaySpace,
        range,
        shuffle = true,
        channelsCreator = const(List("ch1", "ch2")),
        patterns = List(Wildcard, Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      produceMany(
        replaySpace,
        range,
        shuffle = true,
        channelCreator = const("ch1"),
        datumCreator = const("datum1"),
        persist = false
      )
      val replayResults = produceMany(
        replaySpace,
        range,
        shuffle = true,
        channelCreator = const("ch2"),
        datumCreator = const("datum2"),
        persist = false
      )
      val finalPoint = replaySpace.createCheckpoint()

      replayResults should contain theSameElementsAs results
      finalPoint.root shouldBe rigPoint.root
      replaySpace.replayData shouldBe empty
    }

  "Picking 100 datums from 100 waiting datums while doing a bunch of other junk" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      val emptyPoint = space.createCheckpoint()

      produceMany(
        space,
        range = 0 until 100,
        shuffle = false,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      consumeMany(
        space,
        range = 100 until 200,
        shuffle = false,
        channelsCreator = i => List(s"ch$i"),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      produceMany(
        space,
        range = 200 until 300,
        shuffle = false,
        channelCreator = i => s"ch$i",
        datumCreator = i => s"datum$i",
        persist = false
      )
      val results = consumeMany(
        space,
        range = 0 until 100,
        shuffle = false,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      produceMany(
        replaySpace,
        range = 0 until 100,
        shuffle = true,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      consumeMany(
        replaySpace,
        range = 100 until 200,
        shuffle = true,
        channelsCreator = i => List(s"ch$i"),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      produceMany(
        replaySpace,
        range = 200 until 300,
        shuffle = true,
        channelCreator = i => s"ch$i",
        datumCreator = i => s"datum$i",
        persist = false
      )
      val replayResults = consumeMany(
        replaySpace,
        range = 0 until 100,
        shuffle = true,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val finalPoint = replaySpace.createCheckpoint()

      replayResults should contain theSameElementsAs results
      finalPoint.root shouldBe rigPoint.root
      replaySpace.replayData shouldBe empty
    }

  "Picking 100 continuations from 100 persistent waiting continuations while doing a bunch of other junk" should "replay correctly" in
    withTestSpaces { (space, replaySpace) =>
      val emptyPoint = space.createCheckpoint()

      consumeMany(
        space,
        range = 0 until 100,
        shuffle = false,
        channelsCreator = i => List(s"ch$i"),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      produceMany(
        space,
        range = 100 until 200,
        shuffle = false,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      consumeMany(
        space,
        range = 200 until 300,
        shuffle = false,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val results = produceMany(
        space,
        range = 0 until 100,
        shuffle = false,
        channelCreator = i => s"ch$i",
        datumCreator = i => s"datum$i",
        persist = false
      )
      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      consumeMany(
        replaySpace,
        range = 0 until 100,
        shuffle = true,
        channelsCreator = i => List(s"ch$i"),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      produceMany(
        replaySpace,
        range = 100 until 200,
        shuffle = true,
        channelCreator = const("ch1"),
        datumCreator = i => s"datum$i",
        persist = false
      )
      consumeMany(
        replaySpace,
        range = 200 until 300,
        shuffle = true,
        channelsCreator = const(List("ch1")),
        patterns = List(Wildcard),
        continuationCreator = i => s"continuation$i",
        persist = false
      )
      val replayResults = produceMany(
        replaySpace,
        range = 0 until 100,
        shuffle = true,
        channelCreator = i => s"ch$i",
        datumCreator = i => s"datum$i",
        persist = false
      )
      val finalPoint = replaySpace.createCheckpoint()

      replayResults should contain theSameElementsAs results
      finalPoint.root shouldBe rigPoint.root
      replaySpace.replayData shouldBe empty
    }

  "Replay rspace" should "correctly remove things from replay data" in withTestSpaces {
    (space, replaySpace) =>
      val emptyPoint = space.createCheckpoint()

      val channels = List("ch1")
      val patterns = List[Pattern](Wildcard)
      val k        = "continuation"
      val datum    = "datum"

      val cr = Consume.create(channels, patterns, k, persist = false)

      consumeMany(
        space,
        range = 0 to 1,
        shuffle = false,
        channelsCreator = const(channels),
        patterns = patterns,
        continuationCreator = const(k),
        persist = false
      )
      produceMany(
        space,
        range = 0 to 1,
        shuffle = false,
        channelCreator = const(channels(0)),
        datumCreator = const(datum),
        persist = false
      )
      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      val mm: mutable.Map[IOEvent, Multiset[COMM]] = replaySpace.replayData

      mm.get(cr).map(_.size).value shouldBe 2

      replaySpace.consume(channels, patterns, k, persist = false)
      replaySpace.consume(channels, patterns, k, persist = false)
      replaySpace.produce(channels(0), datum, persist = false)

      mm.get(cr).map(_.size).value shouldBe 1

      replaySpace.produce(channels(0), datum, persist = false)

      mm.get(cr) shouldBe None
  }

  "producing" should "return same, stable checkpoint root hashes" in {
    def process(indices: Seq[Int]): Checkpoint = withTestSpaces { (space, replaySpace) =>
      for (i <- indices) {
        replaySpace.produce("ch1", s"datum$i", false)
      }
      space.createCheckpoint()
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

      space.install(key, patterns, continuation)
      replaySpace.install(key, patterns, continuation)

      space.produce(channel, datum, persist = false) shouldBe defined
      val afterProduce = space.createCheckpoint()

      replaySpace.rig(afterProduce.root, afterProduce.log)

      replaySpace.produce(channel, datum, persist = false) shouldBe defined
  }

  "reset" should
    """|empty the replay store,
       |reset the replay trie updates log,
       |and reset the replay data""".stripMargin in
    withTestSpaces { (space, replaySpace) =>
      val channels     = List("ch1")
      val patterns     = List(Wildcard)
      val continuation = "continuation"

      val emptyPoint = space.createCheckpoint()

      space.consume(channels, patterns, continuation, false) shouldBe Right(None)

      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      replaySpace.consume(channels, patterns, continuation, false) shouldBe Right(None)

      val replayStore = replaySpace.store

      replayStore.isEmpty shouldBe false
      replayStore.getTrieUpdates.length shouldBe 1
      replayStore.getTrieUpdateCount shouldBe 1

      replaySpace.reset(emptyPoint.root)
      replayStore.isEmpty shouldBe true
      replayStore.getTrieUpdates.length shouldBe 0
      replayStore.getTrieUpdateCount shouldBe 0
      replaySpace.replayData shouldBe empty

      val checkpoint1 = replaySpace.createCheckpoint()
      checkpoint1.log shouldBe empty
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

      val emptyPoint = space.createCheckpoint()

      space.consume(channels, patterns, continuation, false) shouldBe Right(None)

      val rigPoint = space.createCheckpoint()

      replaySpace.rig(emptyPoint.root, rigPoint.log)

      replaySpace.consume(channels, patterns, continuation, false) shouldBe Right(None)

      val replayStore = replaySpace.store

      replayStore.isEmpty shouldBe false
      replayStore.getTrieUpdates.length shouldBe 1
      replayStore.getTrieUpdateCount shouldBe 1

      val checkpoint0 = replaySpace.createCheckpoint()
      checkpoint0.log shouldBe empty // we don't record trace logs in ReplayRspace

      replaySpace.clear()
      replayStore.isEmpty shouldBe true
      replayStore.getTrieUpdates.length shouldBe 0
      replayStore.getTrieUpdateCount shouldBe 0
      replaySpace.replayData shouldBe empty

      val checkpoint1 = replaySpace.createCheckpoint()
      checkpoint1.log shouldBe empty
    }

  "after close rspace" should "throw RSpaceClosedException on all store operations" in
    withTestSpaces { (space, replaySpace) =>
      val channel      = "ch1"
      val key          = List(channel)
      val patterns     = List(Wildcard)
      val continuation = "continuation"
      val data         = "datum1"

      replaySpace.close()

      an[RSpaceClosedException] shouldBe thrownBy(
        replaySpace.install(key, patterns, continuation)
      )

      an[RSpaceClosedException] shouldBe thrownBy(
        replaySpace.consume(key, patterns, continuation, false)
      )

      an[RSpaceClosedException] shouldBe thrownBy(
        replaySpace.produce(channel, data, false)
      )
    }

  "replay" should "not allow for ambiguous executions" in withTestSpaces { (space, replaySpace) =>
    val noMatch                 = Right(None)
    val empty                   = space.createCheckpoint()
    val channel1                = "ch1"
    val channel2                = "ch2"
    val key1                    = List(channel1, channel2)
    val patterns: List[Pattern] = List(Wildcard, Wildcard)
    val continuation1           = "continuation1"
    val continuation2           = "continuation2"
    val data1                   = "datum1"
    val data2                   = "datum2"
    val data3                   = "datum3"

    //some maliciously 'random' play order
    space.produce(channel1, data3, false, 0) shouldBe noMatch
    space.produce(channel1, data3, false, 0) shouldBe noMatch
    space.produce(channel2, data1, false, 0) shouldBe noMatch

    space.consume(key1, patterns, continuation1, false, 0).right.get should not be empty
    //continuation1 produces data1 on ch2
    space.produce(channel2, data1, false, 1) shouldBe noMatch
    space.consume(key1, patterns, continuation2, false, 0).right.get should not be empty
    //continuation2 produces data2 on ch2
    space.produce(channel2, data2, false, 2) shouldBe noMatch
    val afterPlay = space.createCheckpoint()

    //rig
    replaySpace.rig(empty.root, afterPlay.log)

    //some maliciously 'random' replay order
    replaySpace.produce(channel1, data3, false, 0) shouldBe noMatch
    replaySpace.produce(channel1, data3, false, 0) shouldBe noMatch
    replaySpace.produce(channel2, data1, false, 0) shouldBe noMatch
    replaySpace.consume(key1, patterns, continuation2, false, 0) shouldBe noMatch

    replaySpace.consume(key1, patterns, continuation1, false, 0).right.get should not be empty
    //continuation1 produces data1 on ch2
    replaySpace
      .produce(channel2, data1, false, 1)
      .right
      .get should not be empty //matches continuation2
    //continuation2 produces data2 on ch2
    replaySpace.produce(channel2, data2, false, 1) shouldBe noMatch

    replaySpace.replayData.isEmpty shouldBe true
  }
}

trait ReplayRSpaceTestsBase[C, P, E, A, K] extends FlatSpec with Matchers with OptionValues {
  val logger = Logger(this.getClass.getName.stripSuffix("$"))

  implicit val syncF: Sync[Id] = coop.rchain.catscontrib.effect.implicits.syncId
  implicit val contextShiftF: ContextShift[Id] =
    coop.rchain.rspace.test.contextShiftId

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  def withTestSpaces[S](f: (IdISpace[C, P, E, A, A, K], IReplaySpace[Id, C, P, E, A, A, K]) => S)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      oC: Ordering[C]
  ): S
}

trait LMDBReplayRSpaceTestsBase[C, P, E, A, K] extends ReplayRSpaceTestsBase[C, P, E, A, K] {
  override def withTestSpaces[S](
      f: (IdISpace[C, P, E, A, A, K], IReplaySpace[Id, C, P, E, A, A, K]) => S
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      oC: Ordering[C]
  ): S = {

    implicit val syncF: Sync[Id] = coop.rchain.catscontrib.effect.implicits.syncId
    implicit val contextShiftF: ContextShift[Id] =
      coop.rchain.rspace.test.contextShiftId

    val dbDir       = Files.createTempDirectory("rchain-storage-test-")
    val context     = Context.create[C, P, A, K](dbDir, 1024L * 1024L * 4096L)
    val space       = RSpace.create[Id, C, P, E, A, A, K](context, Branch.MASTER)
    val replaySpace = ReplayRSpace.create[Id, C, P, E, A, A, K](context, Branch.REPLAY)

    try {
      f(space, replaySpace)
    } finally {
      space.close()
      replaySpace.close()
      context.close()
      dbDir.recursivelyDelete()
    }
  }
}

trait MixedReplayRSpaceTestsBase[C, P, E, A, K] extends ReplayRSpaceTestsBase[C, P, E, A, K] {
  override def withTestSpaces[S](
      f: (IdISpace[C, P, E, A, A, K], IReplaySpace[Id, C, P, E, A, A, K]) => S
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      oC: Ordering[C]
  ): S = {

    implicit val syncF: Sync[Id] = coop.rchain.catscontrib.effect.implicits.syncId
    implicit val contextShiftF: ContextShift[Id] =
      coop.rchain.rspace.test.contextShiftId

    val dbDir       = Files.createTempDirectory("rchain-storage-test-")
    val context     = Context.createMixed[C, P, A, K](dbDir, 1024L * 1024L * 4096L)
    val space       = RSpace.create[Id, C, P, E, A, A, K](context, Branch.MASTER)
    val replaySpace = ReplayRSpace.create[Id, C, P, E, A, A, K](context, Branch.REPLAY)

    try {
      f(space, replaySpace)
    } finally {
      space.close()
      replaySpace.close()
      context.close()
      dbDir.recursivelyDelete()
    }
  }
}

trait InMemoryReplayRSpaceTestsBase[C, P, E, A, K] extends ReplayRSpaceTestsBase[C, P, E, A, K] {
  override def withTestSpaces[S](
      f: (IdISpace[C, P, E, A, A, K], IReplaySpace[Id, C, P, E, A, A, K]) => S
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      oC: Ordering[C]
  ): S = {

    val ctx: Context[C, P, A, K] = Context.createInMemory()
    val space                    = RSpace.create[Id, C, P, E, A, A, K](ctx, Branch.REPLAY)
    val replaySpace              = ReplayRSpace.create[Id, C, P, E, A, A, K](ctx, Branch.REPLAY)

    try {
      f(space, replaySpace)
    } finally {
      space.close()
      replaySpace.close()
    }
  }
}

trait FaultyStoreReplayRSpaceTestsBase[C, P, E, A, K] extends ReplayRSpaceTestsBase[C, P, E, A, K] {
  override def withTestSpaces[S](
      f: (IdISpace[C, P, E, A, A, K], IReplaySpace[Id, C, P, E, A, A, K]) => S
  )(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K],
      oC: Ordering[C]
  ): S = {
    implicit val syncF: Sync[Id] = coop.rchain.catscontrib.effect.implicits.syncId
    implicit val contextShiftF: ContextShift[Id] =
      coop.rchain.rspace.test.contextShiftId

    val trieStore = InMemoryTrieStore.create[Blake2b256Hash, GNAT[C, P, A, K]]()
    val mainStore = InMemoryStore
      .create[InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]], C, P, A, K](
        trieStore,
        Branch.REPLAY
      )
    val space = RSpace.create[Id, C, P, E, A, A, K](mainStore, Branch.REPLAY)
    val store =
      new InMemoryStore[InMemTransaction[history.State[Blake2b256Hash, GNAT[C, P, A, K]]], C, P, A, K](
        trieStore,
        Branch.REPLAY
      ) {
        override private[rspace] def createTxnWrite()
          : InMemTransaction[coop.rchain.rspace.State[C, P, A, K]] =
          throw new RuntimeException("Couldn't write to underlying store")
      }

    val replaySpace = new FineGrainedReplayRSpace[Id, C, P, E, A, A, K](store, Branch.REPLAY)

    try {
      f(space, replaySpace)
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
    extends InMemoryReplayRSpaceTestsBase[String, Pattern, Nothing, String, String]
    with ReplayRSpaceTests {}

class FaultyReplayRSpaceTests
    extends FaultyStoreReplayRSpaceTestsBase[String, Pattern, Nothing, String, String] {

  "an exception thrown inside a consume" should "not make replay rspace unresponsive" in
    withTestSpaces { (space, replaySpace) =>
      val channel      = "ch1"
      val key          = List(channel)
      val patterns     = List(Wildcard)
      val continuation = "continuation"

      the[RuntimeException] thrownBy (
        replaySpace.consume(
          key,
          patterns,
          continuation,
          false
        )
      ) should have message "Couldn't write to underlying store"
    }

  "an exception thrown inside a produce" should "not make replay rspace unresponsive" in
    withTestSpaces { (space, replaySpace) =>
      val channel = "ch1"
      val data    = "datum1"

      the[RuntimeException] thrownBy (
        replaySpace.produce(
          channel,
          data,
          false
        )
      ) should have message "Couldn't write to underlying store"
    }
}
