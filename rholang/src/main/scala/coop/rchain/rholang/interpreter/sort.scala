/**
  * Sorts the insides of the Par and ESet/EMap of the rholangADT
  *
  * A score tree is recursively built for each term and is used to sort the insides of Par/ESet/EMap.
  * For most terms, the current term type's absolute value based on the Score object is added as a Leaf
  * to the left most branch and the score tree built for the inside terms are added to the right.
  * The Score object is a container of constants that arbitrarily assigns absolute values to term types.
  * The sort order is total as every term type is assigned an unique value in the Score object.
  * For ground types, the appropriate integer representation is used as the base score tree.
  * For var types, the Debruijn level from the normalization is used.
  *
  * In order to sort an term, call [Type]SortMatcher.sortMatch(term)
  * and extract the .term  of the returned ScoredTerm.
  */
package coop.rchain.rholang.interpreter

import cats.{Applicative, ApplicativeError, Functor, MonadError}
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.rholang.interpreter.errors.{InterpreterError, SortMatchError}
import implicits._
import cats.implicits._
import coop.rchain.catscontrib._

sealed trait Tree[T] {
  def size: Int
}
case class Leaf[T](item: T) extends Tree[T] {
  def size = 1
}
case class Node[T](children: Seq[Tree[T]]) extends Tree[T] {
  def size: Int = children.map(_.size).sum
}

case class ScoreAtom(value: Either[Int, String]) {
  def compare(that: ScoreAtom): Int =
    (this.value, that.value) match {
      case (Left(i1), Left(i2))   => i1.compare(i2)
      case (Left(_), Right(_))    => -1
      case (Right(_), Left(_))    => 1
      case (Right(s1), Right(s2)) => s1.compare(s2)
    }
}

object ScoreAtom {
  def apply(value: Int): ScoreAtom    = new ScoreAtom(Left(value))
  def apply(value: String): ScoreAtom = new ScoreAtom(Right(value))
}

object Leaf {
  def apply(item: Int)    = new Leaf(ScoreAtom(item))
  def apply(item: String) = new Leaf(ScoreAtom(item))
}

object Leaves {
  // Shortcut to be able to write Leaves(1,2,3) instead of Node(Seq(Leaf(1),Leaf(2),Leaf(3)))
  def apply(children: Int*) = new Node(children.map(a => Leaf(ScoreAtom(a))))
}

object Node {
  // Shortcut to write Node(1, Leaf(1)) instead of Node(Seq(Leaf(ScoreAtom(1)), Leaf(ScoreAtom(1))))
  def apply(left: Int, right: Tree[ScoreAtom]*): Tree[ScoreAtom] =
    new Node(Seq(Leaf(left)) ++ right)
}

// Effectively a tuple that groups the term to its score tree.
case class ScoredTerm[T](term: T, score: Tree[ScoreAtom]) extends Ordered[ScoredTerm[T]] {
  def compare(that: ScoredTerm[T]): Int = {
    def compareScore(s1: Tree[ScoreAtom], s2: Tree[ScoreAtom]): Int =
      (s1, s2) match {
        case (Leaf(a), Leaf(b)) => a.compare(b)
        case (Leaf(_), Node(_)) => -1
        case (Node(_), Leaf(_)) => 1
        case (Node(a), Node(b)) =>
          (a, b) match {
            case (Nil, Nil) => 0
            case (Nil, _)   => -1
            case (_, Nil)   => 1
            case (h1 +: t1, h2 +: t2) =>
              compareScore(h1, h2) match {
                case 0     => compareScore(Node(t1), Node(t2))
                case other => other
              }
          }
      }
    compareScore(this.score, that.score)
  }
}

/**
  * Total order of all terms
  *
  * The general order is ground, vars, arithmetic, comparisons, logical, and then others
  */
object Score {
  // For things that are truly optional
  final val ABSENT = 0

  // Ground types
  final val BOOL    = 1
  final val INT     = 2
  final val STRING  = 3
  final val URI     = 4
  final val PRIVATE = 5
  final val ELIST   = 6
  final val ETUPLE  = 7
  final val ESET    = 8
  final val EMAP    = 9

  // Vars
  final val BOUND_VAR = 50
  final val FREE_VAR  = 51
  final val WILDCARD  = 52

  // Expr
  final val EVAR     = 100
  final val ENEG     = 101
  final val EMULT    = 102
  final val EDIV     = 103
  final val EPLUS    = 104
  final val EMINUS   = 105
  final val ELT      = 106
  final val ELTE     = 107
  final val EGT      = 108
  final val EGTE     = 109
  final val EEQ      = 110
  final val ENEQ     = 111
  final val ENOT     = 112
  final val EAND     = 113
  final val EOR      = 114
  final val EMETHOD  = 115
  final val EBYTEARR = 116
  final val EEVAL    = 117

  // Other
  final val QUOTE    = 203
  final val CHAN_VAR = 204

  final val SEND              = 300
  final val RECEIVE           = 301
  final val NEW               = 303
  final val MATCH             = 304
  final val BUNDLE_EQUIV      = 305
  final val BUNDLE_READ       = 306
  final val BUNDLE_WRITE      = 307
  final val BUNDLE_READ_WRITE = 308

  final val CONNECTIVE_NOT = 400
  final val CONNECTIVE_AND = 401
  final val CONNECTIVE_OR  = 402

  final val PAR = 999
}

object BoolSortMatcher {
  def sortMatch(g: GBool): ScoredTerm[GBool] =
    if (g.value) {
      ScoredTerm(g, Leaves(Score.BOOL, 0))
    } else {
      ScoredTerm(g, Leaves(Score.BOOL, 1))
    }
}

object GroundSortMatcher {
  def sortMatch[M[_]](g: ExprInstance)(
      implicit err: MonadError[M, InterpreterError]): M[ScoredTerm[ExprInstance]] =
    g match {
      case gb: GBool   => ScoredTerm(g, BoolSortMatcher.sortMatch(gb).score).pure[M]
      case gi: GInt    => ScoredTerm(g, Leaves(Score.INT, gi.value)).pure[M]
      case gs: GString => ScoredTerm(g, Node(Score.STRING, Leaf(gs.value))).pure[M]
      case gu: GUri    => ScoredTerm(g, Node(Score.URI, Leaf(gu.value))).pure[M]
      case EListBody(gl) =>
        gl.ps.toList
          .traverse(par => ParSortMatcher.sortMatch[M](par))
          .map(pars =>
            ScoredTerm(EListBody(gl.withPs(pars.map(_.term.get))),
                       Node(Score.ELIST, pars.map(_.score): _*)))
      case ETupleBody(gt) =>
        gt.ps.toList
          .traverse(par => ParSortMatcher.sortMatch[M](par))
          .map(pars =>
            ScoredTerm(ETupleBody(gt.withPs(pars.map(_.term.get))),
                       Node(Score.ETUPLE, pars.map(_.score): _*)))
      // Note ESet and EMap rely on the stableness of Scala's sort
      // See https://github.com/scala/scala/blob/2.11.x/src/library/scala/collection/SeqLike.scala#L627
      case ESetBody(gs) =>
        def deduplicate(scoredTerms: Seq[ScoredTerm[Option[Par]]]) =
          scoredTerms.filterNot {
            var set = Set[Par]()
            scoredTerm =>
              {
                val exists = set(scoredTerm.term.get)
                set += scoredTerm.term.get
                exists
              }
          }
        gs.ps.toList
          .traverse(par => ParSortMatcher.sortMatch[M](par))
          .map(_.sorted)
          .map(sortedPars => deduplicate(sortedPars))
          .map(deduplicatedPars =>
            ScoredTerm(ESetBody(gs.withPs(deduplicatedPars.map(_.term.get))),
                       Node(Score.ESET, deduplicatedPars.map(_.score): _*)))
      case EMapBody(gm) =>
        def sortKeyValuePair(kv: KeyValuePair): M[ScoredTerm[KeyValuePair]] =
          for {
            sortedKey   <- ParSortMatcher.sortMatch[M](kv.key)
            sortedValue <- ParSortMatcher.sortMatch[M](kv.value)
          } yield ScoredTerm(KeyValuePair(sortedKey.term, sortedValue.term), sortedKey.score)

        def deduplicateLastWriteWins(scoredTerms: Seq[ScoredTerm[KeyValuePair]]) =
          scoredTerms.reverse.filterNot {
            var set = Set[Par]()
            scoredTerm =>
              {
                val exists = set(scoredTerm.term.key.get)
                set += scoredTerm.term.key.get
                exists
              }
          }.reverse
        gm.kvs.toList
          .traverse(kv => sortKeyValuePair(kv))
          .map(_.sorted)
          .map(sortedPars => deduplicateLastWriteWins(sortedPars))
          .map(deduplicatedPars =>
            ScoredTerm(EMapBody(gm.withKvs(deduplicatedPars.map(_.term))),
                       Node(Score.EMAP, deduplicatedPars.map(_.score): _*)))
      case GByteArray(ba) =>
        ScoredTerm(g, Node(Score.EBYTEARR, Leaf(ba.toString))).pure[M]
      case _ =>
        ApplicativeError[M, InterpreterError].raiseError(
          SortMatchError("GroundSortMatcher passed unknown Expr instance"))
    }
}

object ExprSortMatcher {
  def sortBinaryOperation[M[_]](p1: Option[Par], p2: Option[Par])(
      implicit err: MonadError[M, InterpreterError])
    : M[Tuple2[ScoredTerm[Option[Par]], ScoredTerm[Option[Par]]]] =
    for {
      p1 <- ParSortMatcher.sortMatch[M](p1)
      p2 <- ParSortMatcher.sortMatch[M](p2)
    } yield (p1, p2)

  def sortMatch[M[_]](e: Expr)(
      implicit err: MonadError[M, InterpreterError]): M[ScoredTerm[Expr]] = {
    def constructExpr(exprInstance: ExprInstance, score: Tree[ScoreAtom]) =
      ScoredTerm(Expr(exprInstance = exprInstance), score)
    e.exprInstance match {
      case ENegBody(en) =>
        ParSortMatcher
          .sortMatch[M](en.p)
          .map(sortedPar =>
            constructExpr(ENegBody(ENeg(sortedPar.term)), Node(Score.ENEG, sortedPar.score)))
      case EVarBody(ev) =>
        VarSortMatcher
          .sortMatch[M](ev.v)
          .map(sortedVar =>
            constructExpr(EVarBody(EVar(sortedVar.term)), Node(Score.EVAR, sortedVar.score)))
      case EEvalBody(chan) =>
        ChannelSortMatcher
          .sortMatch[M](chan)
          .map(sortedChan =>
            constructExpr(EEvalBody(sortedChan.term), Node(Score.EEVAL, sortedChan.score)))
      case ENotBody(en) =>
        ParSortMatcher
          .sortMatch[M](en.p)
          .map(sortedPar =>
            constructExpr(ENotBody(ENot(sortedPar.term)), Node(Score.ENOT, sortedPar.score)))
      case EMultBody(em) =>
        sortBinaryOperation[M](em.p1, em.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EMultBody(EMult(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EMULT, sortedPar1.score, sortedPar2.score))
        }
      case EDivBody(ed) =>
        sortBinaryOperation[M](ed.p1, ed.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EDivBody(EDiv(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EDIV, sortedPar1.score, sortedPar2.score))
        }
      case EPlusBody(ep) =>
        sortBinaryOperation[M](ep.p1, ep.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EPlusBody(EPlus(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EPLUS, sortedPar1.score, sortedPar2.score))
        }
      case EMinusBody(em) =>
        sortBinaryOperation[M](em.p1, em.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EMinusBody(EMinus(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EMINUS, sortedPar1.score, sortedPar2.score))
        }
      case ELtBody(el) =>
        sortBinaryOperation[M](el.p1, el.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(ELtBody(ELt(sortedPar1.term, sortedPar2.term)),
                          Node(Score.ELT, sortedPar1.score, sortedPar2.score))
        }
      case ELteBody(el) =>
        sortBinaryOperation[M](el.p1, el.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(ELteBody(ELte(sortedPar1.term, sortedPar2.term)),
                          Node(Score.ELTE, sortedPar1.score, sortedPar2.score))
        }
      case EGtBody(eg) =>
        sortBinaryOperation[M](eg.p1, eg.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EGtBody(EGt(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EGT, sortedPar1.score, sortedPar2.score))
        }
      case EGteBody(eg) =>
        sortBinaryOperation[M](eg.p1, eg.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EGteBody(EGte(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EGTE, sortedPar1.score, sortedPar2.score))
        }
      case EEqBody(ee) =>
        sortBinaryOperation[M](ee.p1, ee.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EEqBody(EEq(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EEQ, sortedPar1.score, sortedPar2.score))
        }
      case ENeqBody(en) =>
        sortBinaryOperation[M](en.p1, en.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(ENeqBody(ENeq(sortedPar1.term, sortedPar2.term)),
                          Node(Score.ENEQ, sortedPar1.score, sortedPar2.score))
        }
      case EAndBody(ea) =>
        sortBinaryOperation[M](ea.p1, ea.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EAndBody(EAnd(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EAND, sortedPar1.score, sortedPar2.score))
        }
      case EOrBody(eo) =>
        sortBinaryOperation[M](eo.p1, eo.p2).map {
          case (sortedPar1, sortedPar2) =>
            constructExpr(EOrBody(EOr(sortedPar1.term, sortedPar2.term)),
                          Node(Score.EOR, sortedPar1.score, sortedPar2.score))
        }
      case EMethodBody(em) =>
        for {
          args         <- em.arguments.toList.traverse(par => ParSortMatcher.sortMatch[M](par))
          sortedTarget <- ParSortMatcher.sortMatch[M](em.target.get)
        } yield
          constructExpr(
            EMethodBody(em.withArguments(args.map(_.term.get)).withTarget(sortedTarget.term.get)),
            Node(
              Seq(Leaf(Score.EMETHOD), Leaf(em.methodName), sortedTarget.score) ++ args.map(
                _.score))
          )
      case eg =>
        GroundSortMatcher
          .sortMatch[M](eg)
          .map(sortedGround => constructExpr(sortedGround.term, sortedGround.score))
    }
  }
}

object VarSortMatcher {
  def sortMatch[M[_]](varOption: Option[Var])(
      implicit err: ApplicativeError[M, InterpreterError]): M[ScoredTerm[Var]] =
    varOption match {
      case Some(v) =>
        v.varInstance match {
          case BoundVar(level) => ScoredTerm(v, Leaves(Score.BOUND_VAR, level)).pure[M]
          case FreeVar(level)  => ScoredTerm(v, Leaves(Score.FREE_VAR, level)).pure[M]
          case Wildcard(_)     => ScoredTerm(v, Leaves(Score.WILDCARD)).pure[M]
        }
      case None => err.raiseError(SortMatchError("VarSortMatcher was passed None"))
    }
}

object ChannelSortMatcher {
  def sortMatch[M[_]: Functor](channelOption: Option[Channel])(
      implicit err: MonadError[M, InterpreterError]): M[ScoredTerm[Channel]] =
    channelOption match {
      case Some(c) =>
        c.channelInstance match {
          case Quote(par) =>
            ParSortMatcher
              .sortMatch[M](par)
              .map(sortedPar =>
                ScoredTerm(Quote(sortedPar.term.get), Node(Score.QUOTE, sortedPar.score)))
          case ChanVar(par) =>
            VarSortMatcher
              .sortMatch[M](par)
              .map(sortedVar =>
                ScoredTerm(ChanVar(sortedVar.term), Node(Score.CHAN_VAR, sortedVar.score)))
        }
      case None => err.raiseError(SortMatchError("ChannelSortMatcher was passed None"))
    }

}

object SendSortMatcher {
  def sortMatch[M[_]](s: Send)(implicit err: MonadError[M, InterpreterError]): M[ScoredTerm[Send]] =
    for {
      sortedChan <- ChannelSortMatcher.sortMatch[M](s.chan)
      sortedData <- s.data.toList.traverse(ParSortMatcher.sortMatch[M](_))
      sortedSend = Send(
        chan = sortedChan.term,
        data = sortedData.map(_.term.get),
        persistent = s.persistent,
        locallyFree = s.locallyFree,
        connectiveUsed = s.connectiveUsed
      )
      persistentScore = if (s.persistent) 1 else 0
      sendScore = Node(
        Score.SEND,
        Seq(Leaf(persistentScore)) ++ Seq(sortedChan.score) ++ sortedData.map(_.score): _*)
    } yield ScoredTerm(sortedSend, sendScore)
}

object ReceiveSortMatcher {
  def sortBind[M[_]](bind: ReceiveBind)(
      implicit err: MonadError[M, InterpreterError]): M[ScoredTerm[ReceiveBind]] = {
    val patterns = bind.patterns
    val source   = bind.source
    for {
      sortedPatterns <- patterns.toList.traverse(channel =>
                         ChannelSortMatcher.sortMatch[M](channel))
      sortedChannel <- ChannelSortMatcher.sortMatch[M](source)
      sortedRemainder <- bind.remainder match {
                          case s @ Some(_) => {
                            VarSortMatcher
                              .sortMatch[M](s)
                              .map(scoredVar => ScoredTerm(Some(scoredVar.term), scoredVar.score))
                          }
                          case None => ScoredTerm(None, Leaf(Score.ABSENT)).pure[M]
                        }
    } yield
      ScoredTerm(
        ReceiveBind(sortedPatterns.map(_.term), sortedChannel.term, bind.remainder, bind.freeCount),
        Node(Seq(sortedChannel.score) ++ sortedPatterns.map(_.score) ++ Seq(sortedRemainder.score))
      )
  }

  // Used during normalize to presort the binds.
  def preSortBinds[M[_], T](
      binds: Seq[Tuple4[Seq[Channel], Channel, Option[Var], DebruijnLevelMap[T]]])(
      implicit err: MonadError[M, InterpreterError])
    : M[Seq[Tuple2[ReceiveBind, DebruijnLevelMap[T]]]] = {
    val sortedBind = binds.toList
      .traverse {
        case (patterns: Seq[Channel],
              channel: Channel,
              remainder: Option[Var],
              knownFree: DebruijnLevelMap[T]) =>
          sortBind[M](
            ReceiveBind(patterns, channel, remainder, freeCount = knownFree.countNoWildcards))
            .map(sortedBind => ScoredTerm((sortedBind.term, knownFree), sortedBind.score))
      }
      .map(_.sorted)
    sortedBind.map(_.map(_.term))
  }

  // The order of the binds must already be presorted by the time this is called.
  // This function will then sort the insides of the preordered binds.
  def sortMatch[M[_]](r: Receive)(
      implicit err: MonadError[M, InterpreterError]): M[ScoredTerm[Receive]] =
    for {
      sortedBinds     <- r.binds.toList.traverse(bind => sortBind[M](bind))
      persistentScore = if (r.persistent) 1 else 0
      sortedBody      <- ParSortMatcher.sortMatch[M](r.body)
    } yield
      ScoredTerm(
        Receive(sortedBinds.map(_.term),
                sortedBody.term,
                r.persistent,
                r.bindCount,
                r.locallyFree,
                r.connectiveUsed),
        Node(Score.RECEIVE,
             Seq(Leaf(persistentScore)) ++
               sortedBinds.map(_.score) ++ Seq(sortedBody.score): _*)
      )
}

object NewSortMatcher {
  def sortMatch[M[_]: Functor](n: New)(
      implicit err: MonadError[M, InterpreterError]): M[ScoredTerm[New]] =
    ParSortMatcher
      .sortMatch[M](n.p)
      .map(sortedPar =>
        ScoredTerm(New(bindCount = n.bindCount, p = sortedPar.term, locallyFree = n.locallyFree),
                   Node(Score.NEW, Leaf(n.bindCount), sortedPar.score)))
}

object MatchSortMatcher {
  def sortMatch[M[_]](m: Match)(
      implicit err: MonadError[M, InterpreterError]): M[ScoredTerm[Match]] = {
    def sortCase(matchCase: MatchCase): M[ScoredTerm[MatchCase]] =
      for {
        sortedPattern <- ParSortMatcher.sortMatch[M](matchCase.pattern)
        sortedBody    <- ParSortMatcher.sortMatch[M](matchCase.source)
      } yield
        ScoredTerm(MatchCase(sortedPattern.term, sortedBody.term, matchCase.freeCount),
                   Node(Seq(sortedPattern.score) ++ Seq(sortedBody.score)))

    for {
      sortedValue <- ParSortMatcher.sortMatch[M](m.target)
      scoredCases <- m.cases.toList.traverse(sortCase)
    } yield
      ScoredTerm(Match(sortedValue.term, scoredCases.map(_.term), m.locallyFree, m.connectiveUsed),
                 Node(Score.MATCH, Seq(sortedValue.score) ++ scoredCases.map(_.score): _*))
  }
}

object BundleSortMatcher {
  def sortMatch[M[_]: Functor](b: Bundle)(
      implicit err: MonadError[M, InterpreterError]): M[ScoredTerm[Bundle]] = {
    val score: Int = if (b.writeFlag && b.readFlag) {
      Score.BUNDLE_READ_WRITE
    } else if (b.writeFlag && !b.readFlag) {
      Score.BUNDLE_WRITE
    } else if (!b.writeFlag && b.readFlag) {
      Score.BUNDLE_READ
    } else {
      Score.BUNDLE_EQUIV
    }
    ParSortMatcher
      .sortMatch[M](b.body)
      .map(sortedPar => ScoredTerm(b.copy(body = sortedPar.term), Node(score, sortedPar.score)))
  }
}

object ConnectiveSortMatcher {
  def sortMatch[M[_]: Functor: Applicative](c: Connective)(
      implicit err: MonadError[M, InterpreterError]): M[ScoredTerm[Connective]] =
    c.connectiveInstance match {
      case ConnAndBody(cb) =>
        cb.ps.toList
          .traverse(par => ParSortMatcher.sortMatch(par)(err))
          .map(pars =>
            ScoredTerm(Connective(ConnAndBody(cb.withPs(pars.map(_.term.get)))),
                       Node(Score.CONNECTIVE_AND, pars.map(_.score): _*)))
      case ConnOrBody(cb) =>
        cb.ps.toList
          .traverse(par => ParSortMatcher.sortMatch(par)(err))
          .map(pars =>
            ScoredTerm(Connective(ConnOrBody(cb.withPs(pars.map(_.term.get)))),
                       Node(Score.CONNECTIVE_OR, pars.map(_.score): _*)))
      case ConnNotBody(p) =>
        ParSortMatcher
          .sortMatch(p)(err)
          .map(scoredPar =>
            ScoredTerm(Connective(ConnNotBody(scoredPar.term.get)),
                       Node(Score.CONNECTIVE_NOT, scoredPar.score)))
    }
}

object ParSortMatcher {
  def sortMatch[M[_]](parOption: Option[Par])(
      implicit err: MonadError[M, InterpreterError]): M[ScoredTerm[Option[Par]]] =
    parOption match {
      case Some(p) =>
        for {
          sends <- p.sends.toList.traverse(s => SendSortMatcher.sortMatch[M](s)).map(_.sorted)
          receives <- p.receives.toList
                       .traverse(r => ReceiveSortMatcher.sortMatch[M](r))
                       .map(_.sorted)
          exprs   <- p.exprs.toList.traverse(e => ExprSortMatcher.sortMatch[M](e)).map(_.sorted)
          news    <- p.news.toList.traverse(n => NewSortMatcher.sortMatch[M](n)).map(_.sorted)
          matches <- p.matches.toList.traverse(m => MatchSortMatcher.sortMatch[M](m)).map(_.sorted)
          bundles <- p.bundles.toList.traverse(b => BundleSortMatcher.sortMatch[M](b)).map(_.sorted)
          connectives <- p.connectives.toList
                          .traverse(c => ConnectiveSortMatcher.sortMatch[M](c))
                          .map(_.sorted)
          ids = p.ids.map(g => ScoredTerm(g, Node(Score.PRIVATE, Leaf(g.id)))).sorted
          sortedPar = Par(
            sends = sends.map(_.term),
            receives = receives.map(_.term),
            exprs = exprs.map(_.term),
            news = news.map(_.term),
            matches = matches.map(_.term),
            bundles = bundles.map(_.term),
            connectives = connectives.map(_.term),
            ids = ids.map(_.term),
            locallyFree = p.locallyFree,
            connectiveUsed = p.connectiveUsed
          )
          parScore = Node(
            Score.PAR,
            sends.map(_.score) ++ receives.map(_.score) ++
              exprs.map(_.score) ++ news.map(_.score) ++
              matches.map(_.score) ++ bundles.map(_.score) ++ ids.map(_.score) ++ connectives.map(
              _.score): _*
          )

        } yield ScoredTerm(sortedPar.pure[Option], parScore)

      case None => err.raiseError(SortMatchError("ParSortMatcher was passed None"))
    }
}
