package coop.rchain.models.rholang.sorter

import cats.effect.Sync
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models._
import cats.implicits._

private[sorter] object GroundSortMatcher extends Sortable[ExprInstance] {
  def sortMatch[F[_]: Sync](g: ExprInstance): F[ScoredTerm[ExprInstance]] =
    g match {
      case gb: GBool =>
        Sortable.sortMatch(gb).map { sorted =>
          ScoredTerm(g, sorted.score)
        }
      case gi: GInt    => ScoredTerm(g, Leaves(Score.INT, gi.value)).pure[F]
      case gs: GString => ScoredTerm(g, Node(Score.STRING, Leaf(gs.value))).pure[F]
      case gu: GUri    => ScoredTerm(g, Node(Score.URI, Leaf(gu.value))).pure[F]
      case GByteArray(ba) =>
        ScoredTerm(g, Node(Score.EBYTEARR, Leaf(ba.toStringUtf8))).pure[F]

      //TODO get rid of Empty nodes in Protobuf unless they represent sth indeed optional
      case Empty =>
        ScoredTerm(g, Node(Score.ABSENT)).pure[F]

      case expr => //TODO(mateusz.gorski): rethink it
        Sync[F].raiseError(
          new IllegalArgumentException(s"GroundSortMatcher passed unknown Expr instance:\n$expr")
        )
    }
}
