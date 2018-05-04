package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.Env._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import implicits._

object Substitute {

  def maybeSubstitute(term: Var)(implicit env: Env[Par]): Either[Var, Par] =
    term.varInstance match {
      case BoundVar(index) =>
        env.get(index) match {
          case Some(par) => Right(par)
          case None      => Left(BoundVar(index))
        }
      case _ => throw new Error(s"Illegal Substitution [$term]")
    }

  def maybeSubstitute(term: EVar)(implicit env: Env[Par]): Either[EVar, Par] =
    maybeSubstitute(term.v.get) match {
      case Left(v)    => Left(EVar(v))
      case Right(par) => Right(par)
    }

  def maybeSubstitute(term: Eval)(implicit env: Env[Par]): Either[Eval, Par] =
    term.channel.get.channelInstance match {
      case Quote(p) => Right(substitute(p))
      case ChanVar(v) =>
        maybeSubstitute(v) match {
          case Left(v)    => Left(Eval(ChanVar(v)))
          case Right(par) => Right(par)
        }
    }

  def substitute(term: Quote)(implicit env: Env[Par]): Quote =
    Quote(substitute(term.value))

  def substitute(term: Bundle)(implicit env: Env[Par]): Bundle = {
    import BundleOps._
    val subBundle = substitute(term.body.get)
    subBundle.singleBundle() match {
      case Some(value) => term.merge(value)
      case None        => term.copy(body = subBundle)
    }
  }

  def substitute(term: Channel)(implicit env: Env[Par]): Channel =
    ChannelSortMatcher
      .sortMatch(
        term.channelInstance match {
          case Quote(p) => Quote(substitute(p))
          case ChanVar(v) =>
            maybeSubstitute(v) match {
              case Left(_v) => ChanVar(_v)
              case Right(p) => Quote(p)
            }
        }
      )
      .term

  def substitute(term: Par)(implicit env: Env[Par]): Par = {

    def subExp(expxs: Seq[Expr]): Par =
      (expxs :\ VectorPar()) { (expr, par) =>
        expr.exprInstance match {
          case EVarBody(e @ EVar(_)) =>
            maybeSubstitute(e) match {
              case Left(_e)    => par.prepend(_e)
              case Right(_par) => _par ++ par
            }
          case e @ _ => par.prepend(substitute(expr))
        }
      }

    def subEval(evals: Seq[Eval]): Par =
      evals.foldRight(VectorPar()) { (eval: Eval, par: Par) =>
        maybeSubstitute(eval) match {
          case Left(plainEval)   => par.prepend(plainEval)
          case Right(droppedPar) => droppedPar ++ par
        }
      }

    ParSortMatcher
      .sortMatch(
        subExp(term.exprs) ++
          subEval(term.evals) ++
          Par(
            evals = Nil,
            exprs = Nil,
            sends = term.sends.map(substitute),
            bundles = term.bundles.map(substitute),
            receives = term.receives.map(substitute),
            news = term.news.map(substitute),
            matches = term.matches.map(substitute),
            ids = term.ids,
            locallyFree = term.locallyFree.until(env.shift),
            connectiveUsed = term.connectiveUsed
          )
      )
      .term
      .get
  }

  def substitute(term: Send)(implicit env: Env[Par]): Send =
    SendSortMatcher
      .sortMatch(
        Send(
          substitute(term.chan.get),
          term.data map { par =>
            substitute(par)
          },
          term.persistent,
          term.locallyFree.until(env.shift),
          term.connectiveUsed
        )
      )
      .term

  def substitute(term: Receive)(implicit env: Env[Par]): Receive =
    ReceiveSortMatcher
      .sortMatch(
        Receive(
          binds = term.binds
            .map({
              case ReceiveBind(xs, Some(chan), rem, freeCount) =>
                ReceiveBind(xs, substitute(chan), rem, freeCount)
            }),
          body = substitute(term.body.get)(env.shift(term.bindCount)),
          persistent = term.persistent,
          bindCount = term.bindCount,
          locallyFree = term.locallyFree.until(env.shift),
          connectiveUsed = term.connectiveUsed
        )
      )
      .term

  def substitute(term: New)(implicit env: Env[Par]): New =
    NewSortMatcher
      .sortMatch(
        New(term.bindCount,
            substitute(term.p.get)(env.shift(term.bindCount)),
            term.locallyFree.until(env.shift))
      )
      .term

  def substitute(term: Match)(implicit env: Env[Par]): Match =
    MatchSortMatcher
      .sortMatch(
        Match(
          target = substitute(term.target.get),
          term.cases.map({
            case MatchCase(_case, Some(par), freeCount) =>
              MatchCase(_case, substitute(par)(env.shift(freeCount)), freeCount)
          }),
          term.locallyFree.until(env.shift),
          term.connectiveUsed
        )
      )
      .term

  def substitute(exp: Expr)(implicit env: Env[Par]): Expr =
    ExprSortMatcher
      .sortMatch(
        exp.exprInstance match {
          case ENotBody(ENot(par))            => ENot(substitute(par.get))
          case ENegBody(ENeg(par))            => ENeg(substitute(par.get))
          case EMultBody(EMult(par1, par2))   => EMult(substitute(par1.get), substitute(par2.get))
          case EDivBody(EDiv(par1, par2))     => EDiv(substitute(par1.get), substitute(par2.get))
          case EPlusBody(EPlus(par1, par2))   => EPlus(substitute(par1.get), substitute(par2.get))
          case EMinusBody(EMinus(par1, par2)) => EMinus(substitute(par1.get), substitute(par2.get))
          case ELtBody(ELt(par1, par2))       => ELt(substitute(par1.get), substitute(par2.get))
          case ELteBody(ELte(par1, par2))     => ELte(substitute(par1.get), substitute(par2.get))
          case EGtBody(EGt(par1, par2))       => EGt(substitute(par1.get), substitute(par2.get))
          case EGteBody(EGte(par1, par2))     => EGte(substitute(par1.get), substitute(par2.get))
          case EEqBody(EEq(par1, par2))       => EEq(substitute(par1.get), substitute(par2.get))
          case ENeqBody(ENeq(par1, par2))     => ENeq(substitute(par1.get), substitute(par2.get))
          case EAndBody(EAnd(par1, par2))     => EAnd(substitute(par1.get), substitute(par2.get))
          case EOrBody(EOr(par1, par2))       => EOr(substitute(par1.get), substitute(par2.get))
          case EListBody(EList(ps, locallyFree, connectiveUsed, rem)) =>
            val _ps = for { par <- ps } yield {
              substitute(par.get)
            }
            val newLocallyFree = locallyFree.until(env.shift)
            Expr(exprInstance = EListBody(EList(_ps, newLocallyFree, connectiveUsed, rem)))
          case ETupleBody(ETuple(ps, locallyFree, connectiveUsed)) =>
            val _ps = for { par <- ps } yield {
              substitute(par.get)
            }
            val newLocallyFree = locallyFree.until(env.shift)
            Expr(exprInstance = ETupleBody(ETuple(_ps, newLocallyFree, connectiveUsed)))
          case ESetBody(ESet(ps, locallyFree, connectiveUsed)) =>
            val _ps = for { par <- ps } yield {
              substitute(par.get)
            }
            val newLocallyFree = locallyFree.until(env.shift)
            Expr(exprInstance = ESetBody(ESet(_ps, newLocallyFree, connectiveUsed)))
          case EMapBody(EMap(kvs, locallyFree, connectiveUsed)) =>
            val _ps = for { KeyValuePair(p1, p2) <- kvs } yield {
              KeyValuePair(substitute(p1.get), substitute(p2.get))
            }
            val newLocallyFree = locallyFree.until(env.shift)
            Expr(exprInstance = EMapBody(EMap(_ps, newLocallyFree, connectiveUsed)))
          case g @ _ => exp
        }
      )
      .term
}
