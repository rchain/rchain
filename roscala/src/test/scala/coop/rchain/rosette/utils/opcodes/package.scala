package coop.rchain.rosette.utils

import coop.rchain.rosette.PC.PLACEHOLDER
import coop.rchain.rosette._

import scala.collection.mutable

package object opcodes {
  val someObs: mutable.Seq[Ob] = mutable.Seq(Ob.NIV, Ob.ABSENT)
  val someTuple: Tuple =
    Tuple(someObs, Ob.NIV +: Ob.NIV +: mutable.Seq.empty[Ob])

  val ctxt = Ctxt(someTuple,
                  null,
                  null,
                  null,
                  null,
                  0,
                  0,
                  PC.PLACEHOLDER,
                  someObs,
                  Ob.NIV,
                  null,
                  null,
                  null +: Ob.NIV +: someObs,
                  Location.LocTrgt)

  val testState = VMState(
    Map.empty,
    Code(someTuple, Ob.NIV +: Ob.NIV +: someObs),
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
