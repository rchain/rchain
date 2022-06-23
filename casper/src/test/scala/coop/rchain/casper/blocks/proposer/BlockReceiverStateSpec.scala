package coop.rchain.casper.blocks.proposer

import coop.rchain.casper.blocks._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BlockReceiverStateSpec extends AnyFlatSpec with Matchers {
  type MId = String

  // beginStored

  "beginStored" should "return true if block is unknown" in {
    val (st, isReceiving) = BlockReceiverState[MId].beginStored("A1")
    st.receiveSt shouldBe Map("A1" -> BeginStoreBlock)
    isReceiving shouldBe true
  }

  "beginStored" should "return false if block status is not Requested" in {
    val (st, _)              = BlockReceiverState[MId].beginStored("A1")
    val (newSt, isReceiving) = st.beginStored("A1")
    newSt.receiveSt shouldBe Map("A1" -> BeginStoreBlock)
    isReceiving shouldBe false
  }

  "beginStored" should "return true if block is Requested" in {
    val (st, _)    = BlockReceiverState[MId].beginStored("A2")
    val (newSt, _) = st.endStored("A2", List("A1" -> true))

    // Unseen parent A1 now have Requested status

    val (_, isReceiving) = newSt.beginStored("A1")
    isReceiving shouldBe true
  }

  // endStored

  "endStored" should "raise AssertionError if current state is not BeginStoreBlock" in {
    val (st, _)    = BlockReceiverState[MId].beginStored("A1")
    val (newSt, _) = st.endStored("A1", List.empty)

    // A1 is now an EndStoreBlock but should be a BeginStoreBlock

    assertThrows[AssertionError](newSt.endStored("A1", List.empty))
  }

  "endStored" should "update state of ending block, state of stored blocks and child relations" in {
    val (st, _)                = BlockReceiverState[MId].beginStored("A2")
    val (newSt, unseenParents) = st.endStored("A2", List("A1" -> true))

    // Status of A2 changed from BeginStoreBlock to EndStoreBlock
    st.receiveSt shouldNot contain("A2" -> EndStoreBlock)
    newSt.receiveSt should contain("A2" -> EndStoreBlock)

    // A1 is unseen parent so it is requested
    st.receiveSt shouldNot contain("A1")
    newSt.receiveSt should contain("A1" -> Requested)
    unseenParents shouldBe Set("A1")

    // Updated child-parent relations
    newSt.blocksSt shouldBe Map("A2"       -> Set("A1"))
    newSt.childRelations shouldBe Map("A1" -> Set("A2"))
  }

  // finished

  "finished" should "raise AssertionError if block not in the state" in {
    assertThrows[AssertionError](BlockReceiverState[MId].finished("A1", Set.empty))
  }

  "finished" should "raise AssertionError if block not received" in {
    val (st, _) = BlockReceiverState[MId].beginStored("A1")
    assertThrows[AssertionError](st.finished("A1", Set.empty))
  }

  "finished" should "return empty state if all blocks are processed" in {
    val (st1, _) = BlockReceiverState[MId].beginStored("A1")
    val (st2, _) = st1.endStored("A1", List.empty)

    // A1 has no dependencies and when it finishes it is removed from the state
    val (st3, _) = st2.finished("A1", Set.empty)

    st3.blocksSt shouldBe empty
    st3.receiveSt shouldBe empty
    st3.childRelations shouldBe empty
  }

  "finished" should "remove resolved deps and return set of blocks with resolved deps" in {
    // Started storing of A2
    val (st1, _) = BlockReceiverState[MId].beginStored("A2")
    st1.receiveSt shouldBe Map("A2" -> BeginStoreBlock)

    // Finished storing of A2 with unseen parent A1
    val (st2, a2UnseenParents) = st1.endStored("A2", List("A1" -> true))
    st2.blocksSt shouldBe Map("A2" -> Set("A1"))
    st2.receiveSt should contain allOf ("A2" -> EndStoreBlock, "A1" -> Requested)
    st2.childRelations shouldBe Map("A1" -> Set("A2"))
    a2UnseenParents shouldBe Set("A1")

    // Started storing of A1
    val (st3, _) = st2.beginStored("A1")
    st3.receiveSt should contain("A1" -> BeginStoreBlock)

    // Finished storing of A1 without parents
    val (st4, a1UnseenParents) = st3.endStored("A1", List.empty)
    st4.blocksSt should contain("A1"  -> Set())
    st4.receiveSt should contain("A1" -> EndStoreBlock)
    a1UnseenParents shouldBe empty

    // After the finishing of A1, it will be removed from receive state
    // Child A2 has dependencies resolved and is pending validation
    val (st5, depsValidated) = st4.finished("A1", Set.empty)
    st5.receiveSt shouldNot contain("A1")
    st5.receiveSt should contain("A2" -> PendingValidation)
    depsValidated shouldBe Set("A2")
  }
}
