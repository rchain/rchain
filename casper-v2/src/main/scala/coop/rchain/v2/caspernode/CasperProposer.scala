package coop.rchain.v2.caspernode
import cats.Applicative
import cats.syntax.all._
import coop.rchain.v2.casper.Casper
import coop.rchain.v2.casper.data.{FinalizationFringe, Justifications}
import coop.rchain.v2.validation.Offence.Slashing

/**
 * Proposes blockchain state changes.
 */
trait CasperProposer[F[_], M, S, B] {

  /**
   * Propose a state change.
   * @param validationFringe  These are messages to be used as justifications.
   *  @param toPropose
   */
  def propose(justifications: Justifications[M, S])
}

object CasperProposer {

  trait ProposeReason
  case class BondedOffenders[M](slashing: Slashing[M]) extends ProposeReason
  case object EpochChange                              extends ProposeReason
  case object StateChange                              extends ProposeReason

  def activeOffenders[F[_]: Applicative, M, S](
      justifications: Justifications[M, S],
      slashings: List[Slashing[M]]
  )(activeValidators: Justifications[M, S] => F[Set[S]], sender: M => S): F[List[Slashing[M]]] =
    activeValidators(justifications).map { av =>
      slashings.filter(s => av.contains(sender(s.message)))
    }

  def noAcquiescence[M, S, ST](justifications: Justifications[M, S])(st: M => ST): Boolean =
    justifications.v.values.toSet.map(st).size > 1

  def shouldPropose[M, S, B](
      justifications: Justifications[M, S],
      finalizationFringe: FinalizationFringe[M]
  )(bondedSenders: B => List[B], seqNum: M => Long, sender: M => S) =
    Casper.lazyMessage(finalizationFringe, justifications)

}
