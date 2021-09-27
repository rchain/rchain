package coop.rchain.casper.v2.stcasper
import coop.rchain.casper.v2.core.SafetyOracle
import coop.rchain.casper.v2.stcasper.syntax.all._

/**
  * Safety oracle for state messages.
  * @tparam U Minimal atomic unit of the state.
  * @tparam S Type of the message sender.
  */
trait StSafetyOracle[F[_], M <: StateMessage[U], U, S] extends SafetyOracle[F, M, S] {
  override def compatible(source: M, target: M): Boolean = !source.conflicts(target)
}
