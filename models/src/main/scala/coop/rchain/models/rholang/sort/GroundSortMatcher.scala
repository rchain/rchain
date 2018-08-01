package coop.rchain.models.rholang.sort

import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{Par, ParMap, ParSet, SortedParHashSet}

private[sort] object GroundSortMatcher extends Sortable[ExprInstance] {
  def sortMatch(g: ExprInstance): ScoredTerm[ExprInstance] =
    g match {
      case gb: GBool   => ScoredTerm(g, Sortable.sortMatch(gb).score)
      case gi: GInt    => ScoredTerm(g, Leaves(Score.INT, gi.value))
      case gs: GString => ScoredTerm(g, Node(Score.STRING, Leaf(gs.value)))
      case gu: GUri    => ScoredTerm(g, Node(Score.URI, Leaf(gu.value)))
      case EListBody(gl) =>
        val sortedPars = gl.ps.toList.map(par => Sortable.sortMatch(par))
        ScoredTerm(EListBody(gl.withPs(sortedPars.map(_.term.get))),
                   Node(Score.ELIST, sortedPars.map(_.score): _*))
      case ETupleBody(gt) =>
        val sortedPars = gt.ps.toList
          .map(par => Sortable.sortMatch(par))
        ScoredTerm(ETupleBody(gt.withPs(sortedPars.map(_.term.get))),
                   Node(Score.ETUPLE, sortedPars.map(_.score): _*))
      // Note ESet and EMap rely on the stableness of Scala's sort
      // See https://github.com/scala/scala/blob/2.11.x/src/library/scala/collection/SeqLike.scala#L627
      case ESetBody(gs) =>
        val sortedPars = gs.ps.sortedPars
          .map(par => Sortable.sortMatch(par))
          .sorted
        ScoredTerm(ESetBody(
                     ParSet(SortedParHashSet(sortedPars.map(_.term.get)),
                            gs.connectiveUsed,
                            gs.locallyFree)),
                   Node(Score.ESET, sortedPars.map(_.score): _*))
      case EMapBody(gm) =>
        def sortKeyValuePair(key: Par, value: Par): ScoredTerm[(Par, Par)] = {
          val sortedKey   = Sortable.sortMatch(key)
          val sortedValue = Sortable.sortMatch(value)
          ScoredTerm((sortedKey.term, sortedValue.term), sortedKey.score)
        }

        val sortedPars = gm.ps.sortedMap.map(kv => sortKeyValuePair(kv._1, kv._2)).sorted
        ScoredTerm(EMapBody(ParMap(sortedPars.map(_.term), gm.connectiveUsed, gm.locallyFree)),
                   Node(Score.EMAP, sortedPars.map(_.score): _*))
      case GByteArray(ba) =>
        ScoredTerm(g, Node(Score.EBYTEARR, Leaf(ba.toString)))
      case _ => //TODO(mateusz.gorski): rethink it
        throw new IllegalArgumentException("GroundSortMatcher passed unknown Expr instance")
    }
}
