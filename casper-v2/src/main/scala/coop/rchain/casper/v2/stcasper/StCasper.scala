package coop.rchain.casper.v2.stcasper
import coop.rchain.casper.v2.core.Casper

/**
  * Casper agreeing on state messages
  * @tparam M Type of the message.
  * @tparam U Minimal atomic unit of the state.
  * @tparam S Type of the message sender.
  */
trait StCasper[F[_], M <: StateMessage[U], U, S] extends Casper[F, M, S]
