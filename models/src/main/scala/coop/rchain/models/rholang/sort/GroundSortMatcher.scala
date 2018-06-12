package coop.rchain.models.rholang.sort

import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.{KeyValuePair, Par, SortedParHashSet}
import cats.implicits._
import cats.syntax._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.ParSet
import coop.rchain.models.rholang.sort.ordering._

object GroundSortMatcher {
  def sortMatch(g: ExprInstance): ScoredTerm[ExprInstance] =
    g match {
      case gb: GBool   => ScoredTerm(g, BoolSortMatcher.sortMatch(gb).score)
      case gi: GInt    => ScoredTerm(g, Leaves(Score.INT, gi.value))
      case gs: GString => ScoredTerm(g, Node(Score.STRING, Leaf(gs.value)))
      case gu: GUri    => ScoredTerm(g, Node(Score.URI, Leaf(gu.value)))
      case EListBody(gl) =>
        val sortedPars = gl.ps.toList.map(par => ParSortMatcher.sortMatch(par))
        ScoredTerm(EListBody(gl.withPs(sortedPars.map(_.term.get))),
                   Node(Score.ELIST, sortedPars.map(_.score): _*))
      case ETupleBody(gt) =>
        val sortedPars = gt.ps.toList
          .map(par => ParSortMatcher.sortMatch(par))
        ScoredTerm(ETupleBody(gt.withPs(sortedPars.map(_.term.get))),
                   Node(Score.ETUPLE, sortedPars.map(_.score): _*))
      // Note ESet and EMap rely on the stableness of Scala's sort
      // See https://github.com/scala/scala/blob/2.11.x/src/library/scala/collection/SeqLike.scala#L627
      case ESetBody(gs) =>
        val sortedPars = gs.ps.sortedPars
          .map(par => ParSortMatcher.sortMatch(par))
          .sorted
        ScoredTerm(ESetBody(
                     ParSet(SortedParHashSet(sortedPars.map(_.term.get)),
                            gs.connectiveUsed,
                            gs.locallyFree)),
                   Node(Score.ESET, sortedPars.map(_.score): _*))
      case EMapBody(gm) =>
        def sortKeyValuePair(kv: KeyValuePair): ScoredTerm[KeyValuePair] = {
          val sortedKey   = ParSortMatcher.sortMatch(kv.key)
          val sortedValue = ParSortMatcher.sortMatch(kv.value)
          ScoredTerm(KeyValuePair(sortedKey.term, sortedValue.term), sortedKey.score)
        }

        def deduplicateLastWins(scoredTerms: Seq[ScoredTerm[KeyValuePair]]) = {
          var set = Set[Par]()
          scoredTerms.reverse.filterNot { scoredTerm =>
            {
              val exists = set(scoredTerm.term.key.get)
              set += scoredTerm.term.key.get
              exists
            }
          }.reverse
        }
        val sortedPars = gm.kvs.toList
          .map(kv => sortKeyValuePair(kv))
          .sorted
        val deduplicatedPars = deduplicateLastWins(sortedPars)
        ScoredTerm(EMapBody(gm.withKvs(deduplicatedPars.map(_.term))),
                   Node(Score.EMAP, deduplicatedPars.map(_.score): _*))
      case GByteArray(ba) =>
        ScoredTerm(g, Node(Score.EBYTEARR, Leaf(ba.toString)))
      case _ => //TODO(mateusz.gorski): rethink it
        throw new IllegalArgumentException("GroundSortMatcher passed unknown Expr instance")
    }
}
