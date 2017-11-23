package coop.rchain.rosette.utils

import coop.rchain.rosette.PC.PLACEHOLDER
import coop.rchain.rosette._

import scala.collection.mutable

package object opcodes {
  val someObs: mutable.Seq[Ob] = mutable.Seq(Ob.NIV, Ob.ABSENT)
  val someTuple: Tuple =
    Tuple(someObs, Ob.NIV +: Ob.NIV +: mutable.Seq.empty[Ob])

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
    monitor = null,
    _slot = null
  )

  val testState = VMState(
    Map.empty,
    Code(someTuple, Seq(), Ob.NIV +: Ob.NIV +: someObs),
    ctxt,
    Location.LocTrgt,
    PLACEHOLDER,
    Seq.empty,
    null,
    systemMonitor = null,
    currentMonitor = null,
    globalEnv = TblObject(someObs, Ob.NIV +: Ob.NIV +: mutable.Seq.empty)
  )
}
