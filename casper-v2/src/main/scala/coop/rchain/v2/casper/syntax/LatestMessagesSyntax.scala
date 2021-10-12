package coop.rchain.v2.casper.syntax
import coop.rchain.v2.casper.data.LatestMessages

trait LatestMessagesSyntax {
  implicit final def LatestMessagesSyntax[F[_], M, S](
      v: LatestMessages[M, S]
  ): LatestMessagesOps[M, S] = new LatestMessagesOps[M, S](v)
}

final class LatestMessagesOps[M, S](val lms: LatestMessages[M, S]) extends AnyVal {
  def toSet: Set[M] = lms.v.values.flatten.toSet

  /**
   * Detects sequence equivocators.
   */
  def seqEquivocatingSenders: Iterable[S] = LatestMessages.sequenceEquivocatingSenders(lms)
}
