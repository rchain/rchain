package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.{Bundle, Expr}

private[sorter] object BundleSortMatcher extends Sortable[Bundle] {
  def sortMatch[F[_]: Sync](b: Bundle): F[ScoredTerm[Bundle]] = {

    val score: Int = if (b.writeFlag && b.readFlag) {
      Score.BUNDLE_READ_WRITE
    } else if (b.writeFlag && !b.readFlag) {
      Score.BUNDLE_WRITE
    } else if (!b.writeFlag && b.readFlag) {
      Score.BUNDLE_READ
    } else {
      Score.BUNDLE_EQUIV
    }
    Sortable
      .sortMatch(b.body)
      .map { sortedPar =>
        ScoredTerm(b.copy(body = sortedPar.term), Node(score, sortedPar.score))
      }
  }
}
