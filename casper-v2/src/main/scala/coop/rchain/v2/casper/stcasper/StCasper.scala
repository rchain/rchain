package coop.rchain.v2.casper.stcasper
import coop.rchain.v2.casper.DependencyGraph

/**
 * Casper agreeing on state messages
 * @tparam M Type of the message.
 * @tparam U Minimal atomic unit of the state.
 * @tparam S Type of the message sender.
 */
trait StCasper[F[_], M <: StateMessage[U], U, S]
    extends StSafetyOracle[F, M, U, S]
    with DependencyGraph[F, M, S]
