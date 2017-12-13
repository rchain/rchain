package coop.rchain.rosette.utils

import coop.rchain.rosette.PC.PLACEHOLDER
import coop.rchain.rosette._

package object opcodes {
  val someObs: Seq[Ob] = Seq(Ob.NIV, Ob.ABSENT)
  val someTuple: Tuple = Tuple(someObs)

  val ctxt = Ctxt(
    tag = Location.LocTrgt,
    nargs = 0,
    outstanding = 0,
    pc = PC.PLACEHOLDER,
    rslt = null,
    trgt = null,
    argvec = someTuple,
    env = null,
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null
  )

  val testState = VMState(
    Map.empty,
    Code(someTuple, Seq()),
    ctxt,
    Location.LocTrgt,
    PLACEHOLDER,
    Seq.empty,
    null,
    systemMonitor = null,
    currentMonitor = null,
    globalEnv = TblObject(someObs)
  )
}
