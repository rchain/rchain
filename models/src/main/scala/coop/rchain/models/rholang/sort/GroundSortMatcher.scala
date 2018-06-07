package coop.rchain.models.rholang.sort

import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.{KeyValuePair, Par, SortedHashSet}
import cats.implicits._
import cats.syntax._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.ParSet
import coop.rchain.models.rholang.sort.ordering._

object GroundSortMatcher {
  def sortMatch(g: ExprInstance): Either[Throwable, ScoredTerm[ExprInstance]] =
    g match {
      case gb: GBool   => ScoredTerm(g, BoolSortMatcher.sortMatch(gb).score).asRight[Throwable]
      case gi: GInt    => ScoredTerm(g, Leaves(Score.INT, gi.value)).asRight[Throwable]
      case gs: GString => ScoredTerm(g, Node(Score.STRING, Leaf(gs.value))).asRight[Throwable]
      case gu: GUri    => ScoredTerm(g, Node(Score.URI, Leaf(gu.value))).asRight[Throwable]
      case EListBody(gl) =>
        gl.ps.toList
          .traverse(par => ParSortMatcher.sortMatch(par))
          .map(pars =>
            ScoredTerm(EListBody(gl.withPs(pars.map(_.term.get))),
                       Node(Score.ELIST, pars.map(_.score): _*)))
      case ETupleBody(gt) =>
        gt.ps.toList
          .traverse(par => ParSortMatcher.sortMatch(par))
          .map(pars =>
            ScoredTerm(ETupleBody(gt.withPs(pars.map(_.term.get))),
                       Node(Score.ETUPLE, pars.map(_.score): _*)))
      // Note ESet and EMap rely on the stableness of Scala's sort
      // See https://github.com/scala/scala/blob/2.11.x/src/library/scala/collection/SeqLike.scala#L627
      case ESetBody(gs) =>
        gs.ps.sortedPars
          .traverse(par => ParSortMatcher.sortMatch(par))
          .map(_.sorted)
          .map(
            sortedPars =>
              ScoredTerm(
                ESetBody(ParSet(SortedHashSet(sortedPars.map(_.term.get)), gs.connectiveUsed)),
                Node(Score.ESET, sortedPars.map(_.score): _*)))
      case EMapBody(gm) =>
        def sortKeyValuePair(kv: KeyValuePair): Either[Throwable, ScoredTerm[KeyValuePair]] =
          for {
            sortedKey   <- ParSortMatcher.sortMatch(kv.key)
            sortedValue <- ParSortMatcher.sortMatch(kv.value)
          } yield ScoredTerm(KeyValuePair(sortedKey.term, sortedValue.term), sortedKey.score)

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
        gm.kvs.toList
          .traverse(kv => sortKeyValuePair(kv))
          .map(_.sorted)
          .map(sortedPars => deduplicateLastWins(sortedPars))
          .map(deduplicatedPars =>
            ScoredTerm(EMapBody(gm.withKvs(deduplicatedPars.map(_.term))),
                       Node(Score.EMAP, deduplicatedPars.map(_.score): _*)))
      case GByteArray(ba) =>
        ScoredTerm(g, Node(Score.EBYTEARR, Leaf(ba.toString))).asRight[Throwable]
      case _ =>
        Left(new IllegalArgumentException("GroundSortMatcher passed unknown Expr instance"))
    }
}
