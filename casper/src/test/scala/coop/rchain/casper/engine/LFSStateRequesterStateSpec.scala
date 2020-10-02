package coop.rchain.casper.engine

import cats.syntax.all._
import coop.rchain.casper.engine.LastFinalizedStateTupleSpaceRequester.ST
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class LFSStateRequesterStateSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

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

  "done" should "make state finished" in {
    val st = ST(Seq(10))

    val st1 = st.done(10)

    st1.isFinished shouldBe true
  }

  "from start to finish" should "receive one item" in {
    val st = ST(Seq(10))

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

    val st4 = st3.done(10)

    // Return finished when all items as Done
    st4.isFinished shouldBe true
  }

}
