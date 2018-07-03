package coop.rchain.rspace

import java.nio.file.Files

import com.google.common.collect.Multiset
import com.typesafe.scalalogging.Logger
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.trace.{COMM, Consume, IOEvent, Produce}
import org.scalatest._

import scala.Function.const
import scala.collection.{immutable, mutable}
import scala.util.Random

//noinspection ZeroIndexToHead,NameBooleanParameters
class ReplayRSpaceTests extends ReplayRSpaceTestsBase[String, Pattern, String, String] {

  def consumeMany[C, P, A, R, K](
      space: ISpace[C, P, A, R, K],
      range: Range,
      shuffle: Boolean,
      channelsCreator: Int => List[C],
      patterns: List[P],
      continuationCreator: Int => K,
      persist: Boolean)(implicit matcher: Match[P, A, R]): List[Option[(K, Seq[R])]] =
    (if (shuffle) Random.shuffle(range.toList) else range.toList).map { i: Int =>
      space.consume(channelsCreator(i), patterns, continuationCreator(i), persist)
    }

  def produceMany[C, P, A, R, K](
      space: ISpace[C, P, A, R, K],
      range: Range,
      shuffle: Boolean,
      channelCreator: Int => C,
      datumCreator: Int => A,
      persist: Boolean)(implicit matcher: Match[P, A, R]): List[Option[(K, immutable.Seq[R])]] =
    (if (shuffle) Random.shuffle(range.toList) else range.toList).map { i: Int =>
      space.produce(channelCreator(i), datumCreator(i), persist)
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

  "Creating a COMM Event that is not contained in the trace log" should "throw a ReplayException" in
    withTestSpaces { (space, replaySpace) =>
      val ch1 = "ch1"

      val initialCheckpoint = space.createCheckpoint()
      replaySpace.rig(initialCheckpoint.root, initialCheckpoint.log)

      replaySpace.consume(List(ch1), List(Wildcard), "continuation", false)

      assertThrows[ReplayException] {
        replaySpace.produce(ch1, data = "datum1", persist = false)
      }
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

      resultConsume shouldBe None
      resultProduce shouldBe defined

      replaySpace.rig(emptyPoint.root, rigPont.log)

      val replayResultConsume = replaySpace.consume(channels, patterns, continuation, false)
      val replayResultProduce = replaySpace.produce(channels(0), datum, false)
      val finalPoint          = space.createCheckpoint()

      replayResultConsume shouldBe None
      replayResultProduce shouldBe resultProduce
      finalPoint.root shouldBe rigPont.root
      replaySpace.getReplayData shouldBe empty
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
      replaySpace.getReplayData shouldBe empty
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
      replaySpace.getReplayData shouldBe empty
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
      replaySpace.getReplayData shouldBe empty
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
      replaySpace.getReplayData shouldBe empty
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
      replaySpace.getReplayData shouldBe empty
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
      replaySpace.getReplayData shouldBe empty
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
      replaySpace.getReplayData shouldBe empty
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
      replaySpace.getReplayData shouldBe empty
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
      replaySpace.getReplayData shouldBe empty
    }

  "consuming" should "correctly remove things from replay data" in withTestSpaces {
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

      val mm: mutable.Map[IOEvent, Multiset[COMM]] = replaySpace.getReplayData

      mm.get(cr).map(_.size).value shouldBe 2

      replaySpace.consume(channels, patterns, k, persist = false)

      mm.get(cr).map(_.size).value shouldBe 1

      replaySpace.consume(channels, patterns, k, persist = false)

      mm.get(cr) shouldBe None
  }

  "producing" should "correctly remove things from replay data" in withTestSpaces {
    (space, replaySpace) =>
      val emptyPoint = space.createCheckpoint()

      val channels = List("ch1")
      val patterns = List[Pattern](Wildcard)
      val k        = "continuation"
      val datum    = "datum"

      val pr = Produce.create(channels(0), datum, persist = false)

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

      val mm: mutable.Map[IOEvent, Multiset[COMM]] = replaySpace.getReplayData

      mm.get(pr).map(_.size).value shouldBe 2

      replaySpace.produce(channels(0), datum, persist = false)

      mm.get(pr).map(_.size).value shouldBe 1

      replaySpace.produce(channels(0), datum, persist = false)

      mm.get(pr) shouldBe None
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
}

trait ReplayRSpaceTestsBase[C, P, A, K]
    extends FlatSpec
    with Matchers
    with OptionValues
    with BeforeAndAfterAll {

  val logger = Logger(this.getClass.getName.stripSuffix("$"))

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  def withTestSpaces[S](f: (RSpace[C, P, A, A, K], ReplayRSpace[C, P, A, A, K]) => S)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]): S = {

    val dbDir       = Files.createTempDirectory("rchain-storage-test-")
    val context     = Context.create[C, P, A, K](dbDir, 1024L * 1024L * 4096L)
    val space       = RSpace.create[C, P, A, A, K](context, Branch.MASTER)
    val replaySpace = ReplayRSpace.create[C, P, A, A, K](context, Branch.REPLAY)

    try {
      f(space, replaySpace)
    } finally {
      space.close()
      replaySpace.close()
      context.close()
    }
  }

}
