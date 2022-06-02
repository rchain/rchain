package coop.rchain.node.mergeablity

import coop.rchain.node.mergeablity.OperationOn0Ch._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers

class JoinMergeability
    extends AnyFlatSpec
    with Matchers
    with Inspectors
    with BasicMergeabilityRules {
  it should "J S S" in ConflictingCase(S0)(S1)(J_)(J_.rstate ++ S0.rstate)(
    J_.rstate ++ S1.rstate
  )
  it should "J S N" in ConflictingCase(S0)(Nil)(J_)(J_.rstate ++ S0.rstate)(J_.rstate)
  it should "J S 4" in ConflictingCase(S0)(F_)(J_)(J_.rstate ++ S0.rstate)(
    J_.rstate ++ F_.rstate
  )
  it should "J S C" in ConflictingCase(S0)(C_)(J_)(J_.rstate ++ S0.rstate)(
    J_.rstate ++ C_.rstate
  )
  it should "J S R" in ConflictingCase(S0)(R1)(J_)(J_.rstate ++ S0.rstate)(
    J_.rstate ++ R1.rstate
  )
  it should "J S P" in ConflictingCase(S0)(P_)(J_)(J_.rstate ++ S0.rstate)(
    J_.rstate ++ P_.rstate
  )
  it should "J 4 4" in MergeableCase(F_)(F1)(J_)(J_.rstate ++ F_.rstate ++ F1.rstate)
  it should "J 4 N" in MergeableCase(F_)(Nil)(J_)(J_.rstate ++ F_.rstate)
  it should "J 4 C" in MergeableCase(F_)(C_)(J_)(J_.rstate ++ F_.rstate ++ C_.rstate)
  it should "J 4 R" in ConflictingCase(F_)(R1)(J_)(J_.rstate ++ F_.rstate)(
    J_.rstate ++ R1.rstate
  )
  it should "J 4 P" in MergeableCase(F_)(P_)(J_)(J_.rstate ++ F_.rstate ++ P_.rstate)
  it should "J C C" in MergeableCase(C_)(C1)(J_)(J_.rstate ++ C_.rstate ++ C1.rstate)
  it should "J C R" in ConflictingCase(C_)(R1)(J_)(J_.rstate ++ C_.rstate)(
    J_.rstate ++ R1.rstate
  )
  it should "J C P" in MergeableCase(C_)(P_)(J_)(J_.rstate ++ C_.rstate ++ P_.rstate)
  it should "J C N" in MergeableCase(C_)(Nil)(J_)(J_.rstate ++ C_.rstate)
  it should "J R R" in ConflictingCase(R0)(R1)(J_)(J_.rstate ++ R0.rstate)(
    J_.rstate ++ R1.rstate
  )
  it should "J R P" in ConflictingCase(R0)(P_)(J_)(J_.rstate ++ R0.rstate)(
    J_.rstate ++ P_.rstate
  )
  it should "J R N" in ConflictingCase(R0)(Nil)(J_)(J_.rstate ++ R0.rstate)(J_.rstate)
  it should "J P P" in MergeableCase(P1)(P0)(J_)(J_.rstate ++ P1.rstate ++ P0.rstate)
  it should "J P N" in MergeableCase(P1)(Nil)(J_)(J_.rstate ++ P1.rstate)
  it should "J N N" in MergeableCase(Nil)(Nil)(J_)(J_.rstate)
  it should "S J J" in MergeableCase(J_)(J_)(S0)(J_.rstate ++ J_.rstate ++ S0.rstate)
  it should "S J S" in ConflictingCase(S0)(J_)(S0)(S0.rstate ++ S0.rstate)(
    J_.rstate ++ S0.rstate
  )
  it should "S J 4" in MergeableCase(F0)(J_)(S0)(J_.rstate)
  it should "S J 4 2" in MergeableCase(F1)(J_)(S0)(J_.rstate ++ F1.rstate ++ S0.rstate)
  it should "S J C" in MergeableCase(C0)(J_)(S0)(C0.rstate ++ J_.rstate)
  it should "S J C 2" in MergeableCase(C1)(J_)(S0)(J_.rstate ++ C1.rstate ++ S0.rstate)
  it should "S J R" in ConflictingCase(R0)(J_)(S0)(R0.rstate ++ S0.rstate)(
    S0.rstate ++ J_.rstate
  )
  it should "S J P" in MergeableCase(P_)(J_)(S0)(J_.rstate ++ S0.rstate)
  it should "S J P 2" in MergeableCase(P1)(J_)(S0)(J_.rstate ++ S0.rstate ++ P1.rstate)
  it should "S J N" in MergeableCase(Nil)(J_)(S0)(J_.rstate ++ S0.rstate)
  it should "4 J J" in MergeableCase(J_)(J_)(F_)(J_.rstate ++ J_.rstate ++ F_.rstate)
  it should "4 J S" in MergeableCase(J_)(S1)(F_)(J_.rstate)
  it should "4 J 4" in MergeableCase(J_)(F1)(F_)(J_.rstate ++ F_.rstate ++ F1.rstate)
  it should "4 J C" in MergeableCase(C0)(J_)(F_)(J_.rstate ++ C0.rstate ++ F_.rstate)
  it should "4 J R" in ConflictingCase(R0)(J_)(F_)(R0.rstate)(F_.rstate ++ J_.rstate)
  it should "4 J R 2" in ConflictingCase(R0)(J_)(F1)(F1.rstate ++ R0.rstate)(
    F1.rstate ++ J_.rstate
  )
  it should "4 J P" in MergeableCase(P_)(J_)(F_)(J_.rstate ++ F_.rstate ++ P_.rstate)
  it should "4 J N" in MergeableCase(Nil)(J_)(F_)(J_.rstate ++ F_.rstate)
  it should "C J J" in MergeableCase(J_)(J_)(C_)(J_.rstate ++ J_.rstate ++ C_.rstate)
  it should "C J S" in MergeableCase(J_)(S1)(C_)(J_.rstate ++ C_.rstate)
  it should "C J 4" in MergeableCase(J_)(F1)(C_)(J_.rstate ++ C_.rstate ++ F1.rstate)
  it should "C J C" in MergeableCase(C0)(J_)(C_)(J_.rstate ++ C0.rstate ++ C_.rstate)
  // it should   "C J R"  in InfiniteLoop(R0)(J_)(C_)(C_.rstate ++ J_.rstate)
  it should "C J R 2" in ConflictingCase(R0)(J_)(C1)(C1.rstate ++ R0.rstate)(
    J_.rstate ++ C1.rstate
  )
  it should "C J P" in MergeableCase(P_)(J_)(C_)(J_.rstate ++ C_.rstate ++ P_.rstate)
  it should "C J N" in MergeableCase(Nil)(J_)(C_)(J_.rstate ++ C_.rstate)
  it should "R J J" in MergeableCase(J_)(J_)(R0)(J_.rstate ++ R0.rstate)
  it should "R J S" in ConflictingCase(J_)(S1)(R0)(J_.rstate ++ R0.rstate)(
    R0.rstate ++ S1.rstate
  )
  it should "R J 4" in MergeableCase(J_)(F1)(R0)(J_.rstate ++ R0.rstate ++ F1.rstate)
  it should "R J C" ignore MergeableCase(C0)(J_)(R0)(J_.rstate ++ R0.rstate)                // infinite loop
  it should "R J C 2" ignore MergeableCase(C1)(J_)(R0)(J_.rstate ++ R0.rstate ++ C1.rstate) // infinite loop
  it should "R J R" in ConflictingCase(R0)(J_)(R0)(R0.rstate ++ R0.rstate)(
    R0.rstate ++ J_.rstate
  ) //???
  it should "R J P" in MergeableCase(P_)(J_)(R0)(J_.rstate ++ R0.rstate)
  it should "R J P 2" in MergeableCase(P1)(J_)(R0)(R0.rstate ++ P1.rstate ++ J_.rstate)
  it should "R J N" in MergeableCase(Nil)(J_)(R0)(J_.rstate ++ R0.rstate)
  it should "P J J" in MergeableCase(J_)(J_)(P_)(J_.rstate ++ J_.rstate ++ P_.rstate)
  it should "P J S" in ConflictingCase(J_)(S1)(P_)(J_.rstate ++ P_.rstate)(S1.rstate)
  it should "P J S 2" in ConflictingCase(J_)(S1)(P0)(J_.rstate ++ P0.rstate)(
    S1.rstate ++ P0.rstate
  )
  it should "P J 4" in MergeableCase(J_)(F1)(P_)(J_.rstate ++ P_.rstate ++ F1.rstate)
  it should "P J C" in MergeableCase(C0)(J_)(P_)(J_.rstate ++ C0.rstate ++ P_.rstate)
  it should "P J R" in ConflictingCase(R0)(J_)(P_)(R0.rstate)(J_.rstate ++ P_.rstate)
  it should "P J R 2" in ConflictingCase(R0)(J_)(P1)(R0.rstate ++ P1.rstate)(
    J_.rstate ++ P1.rstate
  )
  it should "P J P" in MergeableCase(P1)(J_)(P_)(J_.rstate ++ P_.rstate ++ P1.rstate)
  it should "P J N" in MergeableCase(Nil)(J_)(P_)(J_.rstate ++ P_.rstate)
  it should "N J J" in MergeableCase(J_)(J_)(Nil)(J_.rstate ++ J_.rstate)
  it should "N J S" in ConflictingCase(J_)(S1)(Nil)(J_.rstate)(S1.rstate)
  it should "N J 4" in MergeableCase(J_)(F1)(Nil)(J_.rstate ++ F1.rstate)
  it should "N J C" in MergeableCase(C0)(J_)(Nil)(J_.rstate ++ C0.rstate)
  it should "N J R" in ConflictingCase(R0)(J_)(Nil)(R0.rstate)(J_.rstate)
  it should "N J P" in MergeableCase(P1)(J_)(Nil)(J_.rstate ++ P1.rstate)
  it should "N J N" in MergeableCase(Nil)(J_)(Nil)(J_.rstate)
}
