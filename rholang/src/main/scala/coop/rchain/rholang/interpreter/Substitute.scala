package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.Env._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import implicits._

object Substitute {

  def subOrDec(term: Var)(implicit env: Env[Par]): Either[Var, Par] =
    term.varInstance match {
      case BoundVar(level) =>
        env.get(level) match {
          case Some(par) => Right(rename(par, env.level))
          case None =>
            if (level - env.level < 0)
              throw new Error(s"Illegal Index [Variable: $term, Level: ${level - env.level}]")
            else Left(BoundVar(level - env.level))
        }
      case _ => throw new Error(s"Illegal Substitution [$term]")
    }

  def subOrDec(term: EVar)(implicit env: Env[Par]): Either[EVar, Par] =
    subOrDec(term.v.get) match {
      case Left(varue) => Left(EVar(varue))
      case Right(par)  => Right(par)
    }

  def substitute(term: Channel)(implicit env: Env[Par]): Channel =
    ChannelSortMatcher
      .sortMatch(
        term.channelInstance match {
          case Quote(p) => Quote(substitute(p))
          case ChanVar(v) =>
            subOrDec(v) match {
              case Left(_v) => ChanVar(_v)
              case Right(p) => Quote(p)
            }
        }
      )
      .term

  def substitute(term: Par)(implicit env: Env[Par]): Par = {

    def subExp(expxs: List[Expr]): Par =
      (expxs :\ Par()) { (expr, par) =>
        expr.exprInstance match {
          case EVarBody(e @ EVar(_)) =>
            subOrDec(e) match {
              case Left(_e)    => par.prepend(_e)
              case Right(_par) => _par ++ par
            }
          case e @ _ => par.prepend(substitute(expr))
        }
      }

    ParSortMatcher
      .sortMatch(
        Par(
          for { s <- term.sends } yield substitute(s),
          for { r <- term.receives } yield substitute(r),
          for { e <- term.evals } yield substitute(e),
          for { n <- term.news } yield substitute(n),
          Nil,
          for { m <- term.matches } yield substitute(m),
          term.ids,
          term.freeCount,
          term.locallyFree.from(env.level).map(x => x - env.level)
        ) ++ subExp(term.exprs.toList)
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
          term.freeCount,
          term.locallyFree.from(env.level).map(x => x - env.level)
        )
      )
      .term

  def substitute(term: Receive)(implicit env: Env[Par]): Receive =
    ReceiveSortMatcher
      .sortMatch(
        Receive(
          for { ReceiveBind(xs, Some(chan)) <- term.binds } yield {
            ReceiveBind(xs, substitute(chan))
          },
          substitute(term.body.get),
          term.persistent,
          term.bindCount,
          term.freeCount,
          term.locallyFree.from(env.level).map(x => x - env.level)
        )
      )
      .term

  def substitute(term: Eval)(implicit env: Env[Par]): Eval =
    EvalSortMatcher.sortMatch(Eval(substitute(term.channel.get))).term

  def substitute(term: New)(implicit env: Env[Par]): New =
    NewSortMatcher
      .sortMatch(
        New(term.bindCount,
            substitute(term.p.get),
            term.locallyFree.from(env.level).map(x => x - env.level))
      )
      .term

  def substitute(term: Match)(implicit env: Env[Par]): Match =
    MatchSortMatcher
      .sortMatch(
        Match(
          substitute(term.target.get),
          for { MatchCase(_case, Some(par)) <- term.cases } yield {
            MatchCase(_case, substitute(par))
          },
          term.freeCount,
          term.locallyFree.from(env.level).map(x => x - env.level)
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
          case EListBody(EList(ps, freeCount, locallyFree)) =>
            val _ps = for { par <- ps } yield {
              substitute(par.get)
            }
            val newLocallyFree = locallyFree.from(env.level).map(x => x - env.level)
            Expr(exprInstance = EListBody(EList(_ps, freeCount, newLocallyFree)))
          case ETupleBody(ETuple(ps, freeCount, locallyFree)) =>
            val _ps = for { par <- ps } yield {
              substitute(par.get)
            }
            val newLocallyFree = locallyFree.from(env.level).map(x => x - env.level)
            Expr(exprInstance = ETupleBody(ETuple(_ps, freeCount, newLocallyFree)))
          case ESetBody(ESet(ps, freeCount, locallyFree)) =>
            val _ps = for { par <- ps } yield {
              substitute(par.get)
            }
            val newLocallyFree = locallyFree.from(env.level).map(x => x - env.level)
            Expr(exprInstance = ESetBody(ESet(_ps, freeCount, newLocallyFree)))
          case EMapBody(EMap(kvs, freeCount, locallyFree)) =>
            val _ps = for { KeyValuePair(p1, p2) <- kvs } yield {
              KeyValuePair(substitute(p1.get), substitute(p2.get))
            }
            val newLocallyFree = locallyFree.from(env.level).map(x => x - env.level)
            Expr(exprInstance = EMapBody(EMap(_ps, freeCount, newLocallyFree)))
          case g @ _ => exp
        }
      )
      .term

  def rename(term: Var, j: Int): Var =
    term.varInstance match {
      case BoundVar(i) =>
        if (i + j < 0)
          throw new Error(s"Illegal Index [Variable: $term, Level: ${i + j}]")
        else BoundVar(i + j)
      case _ => term
    }

  def rename(term: Channel, j: Int): Channel =
    term.channelInstance match {
      case Quote(par)     => Quote(rename(par, j))
      case ChanVar(varue) => ChanVar(rename(varue, j))
    }

  def rename(term: Par, j: Int): Par =
    Par(
      for (s <- term.sends) yield {
        rename(s, j)
      },
      for (r <- term.receives) yield {
        rename(r, j)
      },
      for (e <- term.evals) yield {
        rename(e, j)
      },
      for (n <- term.news) yield {
        rename(n, j)
      },
      for (e <- term.exprs) yield {
        rename(e, j)
      },
      for (m <- term.matches) yield {
        rename(m, j)
      },
      term.ids,
      term.freeCount,
      term.locallyFree.map(x => x + j)
    )

  def rename(term: Send, j: Int): Send =
    Send(
      rename(term.chan.get, j),
      term.data map { par =>
        rename(par, j)
      },
      term.persistent,
      term.freeCount,
      term.locallyFree.map(x => x + j)
    )

  def rename(term: Receive, j: Int): Receive =
    Receive(
      for (ReceiveBind(xs, channel) <- term.binds) yield {
        ReceiveBind(xs, rename(channel.get, j))
      },
      rename(term.body.get, j),
      term.persistent,
      term.bindCount,
      term.freeCount,
      term.locallyFree.map(x => x + j)
    )

  def rename(term: Eval, j: Int): Eval =
    Eval(rename(term.channel.get, j))

  def rename(term: New, j: Int): New =
    New(term.bindCount, rename(term.p.get, j), term.locallyFree.map(x => x + j))

  def rename(term: Match, j: Int): Match =
    Match(
      rename(term.target.get, j),
      for (MatchCase(pattern, source) <- term.cases) yield {
        MatchCase(pattern, rename(source.get, j))
      },
      term.freeCount,
      term.locallyFree.map(x => x + j)
    )

  def rename(term: Expr, j: Int): Expr =
    term.exprInstance match {
      case EVarBody(EVar(v))          => EVar(rename(v.get, j))
      case ENotBody(ENot(p))          => ENot(rename(p.get, j))
      case ENegBody(ENeg(p))          => ENeg(rename(p.get, j))
      case EMultBody(EMult(p1, p2))   => EMult(rename(p1.get, j), rename(p2.get, j))
      case EDivBody(EDiv(p1, p2))     => EDiv(rename(p1.get, j), rename(p2.get, j))
      case EPlusBody(EPlus(p1, p2))   => EPlus(rename(p1.get, j), rename(p2.get, j))
      case EMinusBody(EMinus(p1, p2)) => EMinus(rename(p1.get, j), rename(p2.get, j))
      case ELtBody(ELt(p1, p2))       => ELt(rename(p1.get, j), rename(p2.get, j))
      case ELteBody(ELte(p1, p2))     => ELte(rename(p1.get, j), rename(p2.get, j))
      case EGtBody(EGt(p1, p2))       => EGt(rename(p1.get, j), rename(p2.get, j))
      case EGteBody(EGte(p1, p2))     => EGte(rename(p1.get, j), rename(p2.get, j))
      case EEqBody(EEq(p1, p2))       => EEq(rename(p1.get, j), rename(p2.get, j))
      case ENeqBody(ENeq(p1, p2))     => ENeq(rename(p1.get, j), rename(p2.get, j))
      case EAndBody(EAnd(p1, p2))     => EAnd(rename(p1.get, j), rename(p2.get, j))
      case EOrBody(EOr(p1, p2))       => EOr(rename(p1.get, j), rename(p2.get, j))
      case EListBody(EList(xs, freeCount, locallyFree)) =>
        EList(for { par <- xs } yield {
          rename(par, j)
        }, freeCount, locallyFree.map(x => x + j))
      case ETupleBody(ETuple(xs, freeCount, locallyFree)) =>
        ETuple(for { par <- xs } yield {
          rename(par, j)
        }, freeCount, locallyFree.map(x => x + j))
      case ESetBody(ESet(xs, freeCount, locallyFree)) =>
        ESet(for { par <- xs } yield {
          rename(par, j)
        }, freeCount, locallyFree.map(x => x + j))
      case EMapBody(EMap(xs, freeCount, locallyFree)) =>
        EMap(for { KeyValuePair(par0, par1) <- xs } yield {
          KeyValuePair(rename(par0.get, j), rename(par1.get, j))
        }, freeCount, locallyFree.map(x => x + j))
      case g @ _ => term
    }
}
