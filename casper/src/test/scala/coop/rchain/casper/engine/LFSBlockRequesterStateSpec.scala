package coop.rchain.casper.engine

import cats.syntax.all._
import coop.rchain.casper.engine.LastFinalizedStateBlockRequester.ST
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class LFSBlockRequesterStateSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "getNext" should "return empty list when called again" in {
    val st = ST(Seq(10))

    // Calling next should produce initial set
    val (st1, ids1) = st.getNext(resend = false)

    ids1 shouldBe Seq(10)

    // Calling next again should NOT return new items
    val (st2, ids2) = st1.getNext(resend = false)

    ids2 shouldBe Seq()

    st1 shouldBe st2
  }

  "getNext" should "return new items after add" in {
    val st = ST(Seq(10))

    // Add new items
    val st2 = st.add(Set(9, 8))

    // Calling next should return new items
    val (_, ids2) = st2.getNext(resend = false)

    ids2 shouldBe Seq(10, 9, 8)
  }

  "received" should "return true for requested and false for unknown" in {
    val st = ST(Seq(10))

    // Mark next as requested
    val (st1, _) = st.getNext(resend = false)

    // Received requested item
    val (_, isReceivedTrue) = st1.received(10)

    isReceivedTrue shouldBe true

    // Received unknown item
    val (_, isReceivedFalse) = st1.received(100)

    isReceivedFalse shouldBe false
  }

  "admitLatest" should "return found and empty if exists in latest" in {
    val st = ST(Seq(10), latest = Set[Int](10))

    // Admit message, check if latest and remove it
    // - returns if latest is found and if latest set is empty
    val (_, (isFound, isEmpty)) = st.admitLatest(10)

    isFound shouldBe true
    isEmpty shouldBe true
  }

  "admitLatest called again" should "return not found" in {
    val st = ST(Seq(10), latest = Set[Int](10))

    val (st1, _) = st.admitLatest(10)

    val (_, (isFound, isEmpty)) = st1.admitLatest(10)

    isFound shouldBe false
    isEmpty shouldBe true
  }

  "done" should "return minimum height if supplied" in {
    val st = ST(Seq(10), latest = Set[Int](10))

    val (_, minHeight) = st.done(10, height = 1000L.some)

    minHeight shouldBe 1000L
  }

  "done" should "return existing minimum height if not supplied" in {
    val st = ST(Seq(10), latest = Set[Int](5))

    val (_, minHeight) = st.done(10, none)

    minHeight shouldBe Long.MaxValue
  }

  "done" should "make state finished" in {
    val st = ST(Seq(10))

    val (st1, _) = st.done(10, none)

    st1.isFinished shouldBe true
  }

  "from start to finish" should "receive one item" in {
    val st = ST(Seq(10), latest = Set[Int](10))

    // Calling next should produce initial set
    val (st1, ids1) = st.getNext(resend = false)

    ids1 shouldBe Seq(10)

    // Calling next again should NOT return new items
    val (st2, ids2) = st1.getNext(resend = false)

    ids2 shouldBe Seq()

    // It should not be finished until all items are Done
    st2.isFinished shouldBe false

    // Received first item
    val (st3, isReceived) = st2.received(10)

    isReceived shouldBe true

    val (st4, _) = st3.done(10, none)

    val (st5, _) = st4.admitLatest(10)

    // Return finished when all items as Done
    st5.isFinished shouldBe true
  }

}
