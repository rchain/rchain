package coop.rchain.v2.casper.stcasper
import coop.rchain.v2.casper.SafetyOracle
import coop.rchain.v2.casper.stcasper.syntax.all._

/**
 * Safety oracle for state messages.
 * @tparam U Minimal atomic unit of the state.
 * @tparam S Type of the message sender.
 */
trait StSafetyOracle[M <: StateMessage[U], U, S] extends SafetyOracle[M, S] {
  override def compatible(source: M, target: M): Boolean = !source.conflicts(target)
}
