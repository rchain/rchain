package coop.rchain.v2.casper.data.syntax
import coop.rchain.v2.casper.data.{FinalizationFringe, LatestMessages}

trait FinalizationFringeSyntax {
  implicit final def FinalizationFringeSyntax[F[_], M, S](
      c: FinalizationFringe[M]
  ): FinalizationFringeOps[M] = new FinalizationFringeOps[M](c)
}

final class FinalizationFringeOps[M](val f: FinalizationFringe[M]) extends AnyVal {
  def checkLazyMessage[S](latestMessages: LatestMessages[M, S], sender: M => S, seqNum: M => Long) =
    f.v.toIterator.map()
}
