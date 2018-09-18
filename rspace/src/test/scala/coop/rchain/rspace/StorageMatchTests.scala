package coop.rchain.rspace
import cats.Monoid
import coop.rchain.rspace.Match.MatchResult
import coop.rchain.rspace.Match.MatchResult.{Error, Found, NotFound}
import coop.rchain.rspace.StorageMatchTests._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringMatch, StringsCaptor, Wildcard}
import coop.rchain.rspace.internal.{Datum, GNAT, WaitingContinuation}
import coop.rchain.rspace.util._
import org.scalactic.TripleEqualsSupport
import scodec.Codec

import scala.collection.immutable.{Seq => ImmSeq}

trait StorageMatchTests
    extends StorageTestsBase[String, Pattern, EmptyStringError.type, Int, String, StringsCaptor]
    with TestImplicitHelpers
    with TripleEqualsSupport {

  implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
  implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
  implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

  implicit val matcher: Match[Pattern, EmptyStringError.type, String, Int, String] = storageMatch
  type TestProduceMap = Map[String, Datum[String]]

  type TestConsumeMap = Map[List[String], WaitingContinuation[Pattern, StringsCaptor]]

  type TestGNAT = GNAT[String, Pattern, String, StringsCaptor]

  private implicit class FoundCastOps(
      matchResult: cats.Id[
        MatchResult[(StringsCaptor, ImmSeq[String]), Int, EmptyStringError.type]]) {
    def asFound: Found[StringsCaptor, Int, EmptyStringError.type] =
      matchResult.asInstanceOf[Found[StringsCaptor, Int, EmptyStringError.type]]
  }

  behavior of "matching algorithm"

  "produce on empty tuplespace" should "return empty state" in withTestSpace { space =>
    val key  = List("ch1")
    val data = "datum"

    val r1 = space.produce(key.head, data, persist = false)
    assert(r1.isNotFound)
    assert(r1 === NotFound(0))
  }

  "produce and then consume from the same channel with a matching pattern" should
    "return a cost along the matching data" in withTestSpace { space =>
    val key  = List("ch1")
    val data = "datum"

    val r1 = space.produce(key.head, data, persist = false)
    assert(r1.isNotFound)
    assert(r1 === NotFound(0))

    val r2 = space.consume(key, List(Wildcard), new StringsCaptor, persist = false)
    assert(r2.isFound)
    assert(r2.asFound.state === data.length)

    runK(r2.toEither)
    assert(getK(r2.toEither).results === List(List("datum")))
  }

  "produce and then consume with a pattern that doesn't match" should
    "return a cost despite of not finding a match" in withTestSpace { space =>
    val key  = List("ch1")
    val data = "datum"

    val r1 = space.produce(key.head, data, persist = false)
    assert(r1.isNotFound)
    assert(r1 === NotFound(0))

    val r2 = space.consume(key, List(StringMatch("no-match")), new StringsCaptor, persist = false)
    assert(r2.isNotFound)
    assert(r2 === NotFound(data.length))
  }

  "Consuming three times with different patterns and then producing that doesn't match any" should
    "return an accumulated cost of matching along with NotFound status" in withTestSpace { space =>
    val ch1 = "ch1"

    val datum1 = "datum1"
    val datum2 = "datum2"
    val datum3 = "datum3"

    val r1 = space.consume(List(ch1), List(StringMatch(datum1)), new StringsCaptor, persist = false)
    val r2 = space.consume(List(ch1), List(StringMatch(datum2)), new StringsCaptor, persist = false)
    val r3 = space.consume(List(ch1), List(StringMatch(datum3)), new StringsCaptor, persist = false)

    assert(r1.isNotFound)
    assert(r1 === NotFound(0))

    assert(r2.isNotFound)
    assert(r2 === NotFound(0))

    assert(r3.isNotFound)
    assert(r3 === NotFound(0))

    val datum4 = "datum4"
    // it will try to match will all 3 patterns on the channel but none will match
    val r4 = space.produce(ch1, datum4, persist = false)

    assert(r4.isNotFound)
    assert(r4 === NotFound(datum4.length * 3))
  }

  "produce and then consume with a pattern that ends with an error" should
    "return an error and a cost of a match" in withTestSpace { space =>
    val key       = List("ch1")
    val emptyData = ""

    val r1 = space.produce(key.head, emptyData, persist = false)
    assert(r1.isNotFound)
    assert(r1 === NotFound(0))

    val r2 = space.consume(key, List(StringMatch("no-match")), new StringsCaptor, persist = false)
    assert(r2.isError)
    assert(r2 === Error(100, EmptyStringError))
  }

  "produce, produce and then consume" should "return accumulated cost of all matchings" in withTestSpace {
    space =>
      val ch1   = "ch1"
      val data1 = "data1"

      val r1 = space.produce(ch1, data1, persist = false)
      assert(r1.isNotFound)
      assert(r1 === NotFound(0))

      val data2 = "data2"
      val r2    = space.produce(ch1, data2, persist = false)
      assert(r2.isNotFound)
      assert(r2 === NotFound(0))

      val r3 =
        space.consume(List(ch1), List(StringMatch("no-match")), new StringsCaptor, persist = false)
      assert(r3.isNotFound)
      assert(r3 === NotFound(data1.length + data2.length))
  }

  "A joined consume with the same channel given twice followed by a produce" should
    "return accumulated cost of matching" in withTestSpace { space =>
    val channels = List("ch1", "ch1")

    val datum1  = "datum1"
    val datum11 = "datum11"
    val r1 = space.consume(channels,
                           List(StringMatch(datum1), StringMatch(datum11)),
                           new StringsCaptor,
                           persist = false)

    val r2 = space.produce("ch1", datum11, persist = false)
    val r3 = space.produce("ch1", datum1, persist = false)

    assert(r1.isNotFound)
    assert(r2.isNotFound)
    assert(r3.isFound)
    assert(r3.asFound.state === datum1.length + datum11.length)
  }

  "A joined consume with different channels followed by a produce" should
    "return accumulated cost of matching along the data" in withTestSpace { space =>
    val channels = List("ch1", "ch2")

    val datum1  = "datum1"
    val datum12 = "datum11"
    val r1 = space.consume(channels,
                           List(StringMatch(datum1), StringMatch(datum12)),
                           new StringsCaptor,
                           persist = false)

    val r2 = space.produce("ch2", datum12, persist = false)
    val r3 = space.produce("ch1", datum1, persist = false)

    assert(r1.isNotFound)
    assert(r2.isNotFound)
    assert(r3.isFound)
    assert(r3.asFound.state === datum1.length + datum12.length)
  }

  "A joined consume with different channels, where one of them fails, followed by a produce" should
    "return accumulated cost of matching along the error" in withTestSpace { space =>
    val ch1    = "ch1"
    val ch2    = "ch2"
    val datum1 = "datum1"

    val channelsWithData     = List((ch1, StringMatch(datum1)), (ch2, StringMatch("will-error")))
    val (channels, patterns) = channelsWithData.unzip

    val r1 = space.consume(channels, patterns, new StringsCaptor, persist = false)

    //TODO: This is an incorrect behavior but the one that we have to live with now
    //we shouldn't compare joined receive when we have produce on a single channel
    //because we errored when doing that the produce doesn't make to the tuplespace
    //and we won't have a match in the end (because we now only have produce over one channel instead of two joined ones)
    val r2 = space.produce(ch2, ERROR_STRING, persist = false)
    val r3 = space.produce(ch1, datum1, persist = false)
    assert(r1.isNotFound)
    assert(r1 === NotFound(0))
    assert(r2.isError)
    assert(r2 === Error(MATCH_ERROR_COST, EmptyStringError))
    assert(r3.isNotFound)
    assert(r3 === NotFound(datum1.length))
  }

  "A joined consume with different channels, where none of them matches, followed by a produce" should
    "return accumulated cost along the NotFound state" in withTestSpace { space =>
    val ch1 = "ch1"
    val ch2 = "ch2"

    val r1 = space.consume(List(ch1, ch2),
                           List(StringMatch("pattern1"), StringMatch("pattern2")),
                           new StringsCaptor,
                           persist = false)
    assert(r1.isNotFound)

    val datum1 = "datum1"
    val r2     = space.produce(ch1, datum1, persist = false)
    assert(r2.isNotFound)
    val datum2 = "datum2"
    val r3     = space.produce(ch2, datum2, persist = false)
    assert(r3.isNotFound)
    assert(r3 === NotFound(datum1.length + datum2.length))
  }

  "Consuming three times on the same channel, with different patterns, and then producing" should
    "return accumulated cost of matching" in withTestSpace { space =>
    val ch1 = "ch1"

    val datum1 = "datum1"
    val datum2 = "datum2"
    val datum3 = "datum3"

    // we need to run it few times because the order in which rspace compares patterns with data
    // is not deterministic
    val test = (0 to 5)
      .map { _ =>
        val r1 =
          space.consume(List(ch1), List(StringMatch(datum1)), new StringsCaptor, persist = false)
        val r2 =
          space.consume(List(ch1), List(StringMatch(datum2)), new StringsCaptor, persist = false)
        val r3 =
          space.consume(List(ch1), List(StringMatch(datum3)), new StringsCaptor, persist = false)

        assert(r1.isNotFound)
        assert(r1 === NotFound(0))

        assert(r2.isNotFound)
        assert(r2 === NotFound(0))

        assert(r3.isNotFound)
        assert(r3 === NotFound(0))

        val r4 = space.produce(ch1, "datum1", persist = false)
        assert(r4.isFound)
        val found = r4.asFound
        space.clear()
        found.state > "datum1".length
      }
      .contains(true)
    assert(test)
  }

  "Producing three times on the same channel, with different patterns where one of the matches fails, and then consuming" should
    "return accumulated cost of matching along with the error" in withTestSpace { space =>
    val ch1 = "ch1"

    val datum1 = "datum1"
    val datum2 = "datum2"

    // we need to run it few times because the order in which rspace compares patterns with data
    // is not deterministic
    val test = (0 to 5)
      .map { _ =>
        val r1 = space.produce(ch1, datum1, persist = false)
        val r2 = space.produce(ch1, datum2, persist = false)
        val r3 = space.produce(ch1, ERROR_STRING, persist = false)

        assert(r1.isNotFound)
        assert(r1 === NotFound(0))

        assert(r2.isNotFound)
        assert(r2 === NotFound(0))

        assert(r3.isNotFound)
        assert(r3 === NotFound(0))

        val r4 =
          space.consume(List(ch1), List(StringMatch("datum3")), new StringsCaptor, persist = false)
        assert(r4.isError)
        space.clear()
        r4
      }
      .collect {
        // we are looking for a case when the errored matching is happening 2nd or 3rd
        // we want to test that RSpace accumulates costs of matches when error happens later in the algorithm
        case Error(s, _) if s > 100 => true
      }
      .contains(true)
    assert(test)
  }

}

class InMemoryStorageMatchTests
    extends InMemoryStoreTestsBase[EmptyStringError.type, Int]()(integerAddMonoid)
    with StorageMatchTests

class LmdbStorageMatchTests
    extends LMDBStoreTestsBase[EmptyStringError.type, Int]()(integerAddMonoid)
    with StorageMatchTests

class MixedStorageMatchTests
    extends MixedStoreTestsBase[EmptyStringError.type, Int]()(integerAddMonoid)
    with StorageMatchTests

object StorageMatchTests {
  private[rspace] case object EmptyStringError

  // let's add an unproportionally big cost for an error
  // so it's easy to distinguish in test
  val MATCH_ERROR_COST = 100
  val ERROR_STRING     = ""

  implicit object integerAddMonoid extends Monoid[Int] {
    override def empty: Int                   = 0
    override def combine(x: Int, y: Int): Int = x + y
  }

  implicit object storageMatch extends Match[Pattern, EmptyStringError.type, String, Int, String] {
    override def get(p: Pattern, a: String): Match.MatchResult[String, Int, EmptyStringError.type] =
      if (a == ERROR_STRING)
        Error(MATCH_ERROR_COST, EmptyStringError)
      else
        Some(a).filter(p.isMatch) match {
          case None      => NotFound(a.length)
          case Some(mat) => Found(a.length, mat)
        }
  }
}
