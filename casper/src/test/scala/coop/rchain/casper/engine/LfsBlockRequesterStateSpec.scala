package coop.rchain.casper.engine

import cats.syntax.all._
import coop.rchain.casper.engine.LfsBlockRequester.{ReceiveInfo, ST}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class LfsBlockRequesterStateSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "getNext" should "return empty list when called again" in {
    val st = ST(Set(10))

    // Calling next should produce initial set
    val (st1, ids1) = st.getNext(resend = false)

    ids1 shouldBe Seq(10)

    // Calling next again should NOT return new items
    val (st2, ids2) = st1.getNext(resend = false)

    ids2 shouldBe Seq()

    st1 shouldBe st2
  }

  "getNext" should "return new items after add" in {
    val st = ST(Set(10))

    // Add new items
    val st2 = st.add(Set(9, 8))

    // Calling next should return new items
    val (_, ids2) = st2.getNext(resend = false)

    ids2 shouldBe Seq(10, 9, 8)
  }

  "getNext" should "return requested items on resend" in {
    val st = ST(Set(10))

    // Calling next should return new items
    val (st1, ids1) = st.getNext(resend = false)

    ids1 shouldBe Seq(10)

    // Calling next with resend should return already requested
    val (_, ids2) = st1.getNext(resend = true)

    ids2 shouldBe Seq(10)
  }

  "isRequested" should "return flag if item is added to requested items" in {
    val st = ST(Set(10))

    // Calling next to request added items
    val (st1, _) = st.getNext(resend = false)

    // Check requested item
    val requested = st1.d.contains(10)

    requested shouldBe true

    // Check not requested item
    val requested1 = st1.d.contains(100)

    requested1 shouldBe false
  }

  "received" should "return true for requested and false for unknown" in {
    val st = ST(Set(10))

    // Mark next as requested
    val (st1, _) = st.getNext(resend = false)

    // Received requested item
    val (_, ReceiveInfo(requested, _, _)) = st1.received(10, 100)

    requested shouldBe true

    // Received unknown item
    val (_, ReceiveInfo(requested1, _, _)) = st1.received(100, 200)

    requested1 shouldBe false
  }

  "received" should "return flag based on calculated height" in {
    val st = ST(Set(10, 11), latest = Set(10), lowerBound = 200)

    // Mark next as requested
    val (st1, _) = st.getNext(resend = false)

    // Received the last latest item (sets minimum height)
    val (st2, receiveInfo1) = st1.received(10, 100)

    receiveInfo1 shouldBe ReceiveInfo(requested = true, latest = true, lastlatest = true)

    // Minimum height should be recalculated based on the last latest item (-1)
    st2.lowerBound shouldBe 99

    // Mark next as requested
    val (st3, ids2) = st2.getNext(resend = false)

    ids2 shouldBe Seq(11)

    // Received higher height should be accepted
    val (st4, ReceiveInfo(requested3, _, _)) = st3.received(11, 50)

    requested3 shouldBe true

    // Minimum height should stay the same after all latest items received
    st4.lowerBound shouldBe 99
  }

  "received" should "return next only after latest are received" in {
    val st = ST(Set(10, 11, 12), latest = Set(10, 11))

    // Mark next as requested
    val (st1, _) = st.getNext(resend = false)

    // Received latest item
    val (st2, receiveInfo) = st1.received(10, 100)

    receiveInfo shouldBe ReceiveInfo(requested = true, latest = true, lastlatest = false)

    // Before all latest received, next should be empty
    val (st3, ids1) = st2.getNext(resend = false)

    ids1 shouldBe Seq.empty

    // Received latest item (the last one)
    val (st4, receiveInfo1) = st3.received(11, 110)

    receiveInfo1 shouldBe ReceiveInfo(requested = true, latest = true, lastlatest = true)

    // After the last of latest received, the rest of items should be requested
    val (_, ids4) = st4.getNext(resend = false)

    ids4 shouldBe Seq(12)
  }

  "done" should "make state finished" in {
    val st = ST(Set(10))

    // If item is not received, it should stay unfinished
    val st1 = st.done(10)

    st1.isFinished shouldBe false

    // Mark next as requested ...
    val (st2, _) = st1.getNext(resend = false)
    // ... and received
    val (st3, _) = st2.received(10, 100)

    val st4 = st3.done(10)

    st4.isFinished shouldBe true
  }

  "from start to finish" should "receive one item" in {
    val st = ST(Set(10), latest = Set[Int](10))

    // Calling next should produce initial set
    val (st1, ids1) = st.getNext(resend = false)

    ids1 shouldBe Seq(10)

    // Calling next again should NOT return new items
    val (st2, ids2) = st1.getNext(resend = false)

    ids2 shouldBe Seq()

    // It should not be finished until all items are Done
    st2.isFinished shouldBe false

    // Received first item
    val (st3, ReceiveInfo(requested, _, _)) = st2.received(10, 100)

    requested shouldBe true

    val st4 = st3.done(10)

    // Return finished when all items as Done
    st4.isFinished shouldBe true
  }

}
