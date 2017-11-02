package coop.rchain.rosette.utils

import coop.rchain.rosette.PC.PLACEHOLDER
import coop.rchain.rosette._

package object opcodes {
  val someObs: Seq[Ob] = Seq(Ob.NIV, Ob.ABSENT)
  val someTuple: Tuple = Tuple(someObs, Ob.NIV, Ob.NIV, Seq.empty)

  val ctxt = Ctxt(someTuple,
                  null,
                  null,
                  null,
                  null,
                  null,
                  0,
                  0,
                  Ob.NIV,
                  PC.PLACEHOLDER,
                  someObs,
                  Ob.NIV,
                  null,
                  null,
                  someObs,
                  Location.LocTrgt)

  val testState = VMState(
    Map.empty,
    Code(someTuple, Ob.NIV, Ob.NIV, someObs),
    ctxt,
    Location.LocTrgt,
    PLACEHOLDER,
    Seq.empty,
    null,
    systemMonitor = null,
    currentMonitor = null,
    globalEnv = TblObject(someObs, Ob.NIV, Ob.NIV, Seq.empty)
  )
}
