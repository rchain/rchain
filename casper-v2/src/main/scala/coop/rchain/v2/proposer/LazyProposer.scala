package coop.rchain.v2.proposer
import cats.Applicative
import cats.syntax.all._
import coop.rchain.v2.casper.data.Justifications
import coop.rchain.v2.validation.Offence.Slashing

/**
 * Rules for "lazy" proposing, i.e. offering a message only under minimal set of conditions.
 * Network in which lazy proposing is enabled is silent if no users are making transactions.
 */
object LazyProposer {

  /**
   * In the base blockchain state offenders that are detected by the node are still active.
   *    So node have to propose slashing.
   */
  def activeOffenders[F[_]: Applicative, M, S](
      justifications: Justifications[M, S],
      slashings: List[Slashing[M]]
  )(activeValidators: Justifications[M, S] => F[Set[S]], sender: M => S): F[List[Slashing[M]]] =
    activeValidators(justifications).map { av =>
      slashings.filter(s => av.contains(sender(s.message)))
    }

  /**
   * Casper scope exposes that there is no acquiescence on the block chain state, and merge should be performed.
   */
  def noAcquiescence[M, S, ST](justifications: Justifications[M, S])(st: M => ST): Boolean =
    justifications.v.values.toSet.map(st).size > 1

  /**
   * Block to be proposed is epoch change block.
   */
  def epochChangeBlock(blockNum: Long, epochLength: Long): Boolean = epochLength % blockNum == 0

  /**
   * There are transactions pending to be included in a block.
   */
  def transactionsPending[T](transactionsPending: List[T]): Boolean = transactionsPending.nonEmpty

}
