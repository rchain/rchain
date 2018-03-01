package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.Env.DeBruijn
import coop.rchain.rholang.interpreter.Substitute.substitute

import scala.collection.immutable.HashMap

sealed trait Substitute

object Substitute {

  def substitute(term: Par, env: Env[Par]): Par = {
    def subExp(expxs: List[Expr]): Par =
      (Par() /: expxs) { (par, expr) =>
        expr match {
          case e @ EVar(_) =>
            subOrDec(e, env) match {
              case Left(_e)    => par :+ _e
              case Right(_par) => par ++ _par
            }
          case e @ _ => par :+ substitute(e, env)
        }
      }
    val sends = for { send <- term.sends } yield {
      substitute(send, env)
    }
    val recvs = for { recv <- term.receives } yield {
      substitute(recv, env)
    }
    val evals = for { eval <- term.evals } yield {
      substitute(eval, env)
    }
    val news = for { neu <- term.news } yield {
      substitute(neu, env)
    }
    val matches           = term.matches
    val exprs: List[Expr] = List()
    val freeCount: Int =
      (0 /: sends)((i, s) => i + s.freeCount) +
        (0 /: recvs)((i, r) => i + r.freeCount)
        (0 /: evals)((i, e) => i + e.freeCount)
        (0 /: news)((i, n) => i + n.freeCount)
        (0 /: matches)((i, m) => i + m.freeCount)

    Par(
      sends,
      recvs,
      evals,
      news,
      exprs,
      matches,
      term.id,
      freeCount
    ) ++ subExp(term.exprs)
  }

  def substitute(term: Quote, env: Env[Par]): Quote = Quote(substitute(term.p, env))

  def substitute(term: Channel, env: Env[Par]): Channel =
    term match {
      case q @ Quote(_) => substitute(q, env)
      case ChanVar(b @ _) =>
        subOrDec(b, env) match {
          case Left(varue) => ChanVar(varue)
          case Right(par)  => throw new Error(s"Illegal Substitution [$par/$term]")
        }
    }

  def substitute(term: Send, env: Env[Par]): Send = {
    val _data = term.data map { par =>
      substitute(par, env)
    }
    val freeCount = term.chan.freeCount + (0 /: _data) { (i, par) =>
      i + par.freeCount
    }
    Send(
      substitute(term.chan, env),
      _data,
      term.persistent,
      freeCount
    )
  }

  def substitute(term: Eval, env: Env[Par]): Eval =
    Eval(substitute(term.channel, env))

  def substitute(term: New, env: Env[Par]): New =
    New(term.bindCount, substitute(term.p, env))

  def substitute(term: Receive, env: Env[Par]): Receive = {
    val _binds = for { (xs, chan) <- term.binds } yield {
      (xs, substitute(chan, env))
    }
    val freeCount = (0 /: _binds) {
      case (i, (chanxs, chan)) =>
        i + chan.freeCount + (0 /: chanxs) { (j, _chan) =>
          j + _chan.freeCount
        }
    }
    Receive(
      _binds,
      substitute(term.body, env),
      term.persistent,
      _binds.size,
      freeCount
    )
  }

  def subOrDec(term: Var, env: Env[Par]): Either[Var, Par] =
    term match {
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

  def subOrDec(exp: EVar, env: Env[Par]): Either[EVar, Par] =
    subOrDec(exp.v, env) match {
      case Left(varue) => Left(EVar(varue))
      case Right(par)  => Right(par)
    }

  def substitute(exp: Expr, env: Env[Par]): Expr =
    exp match {
      case ENot(par)          => ENot(substitute(par, env))
      case ENeg(par)          => ENeg(substitute(par, env))
      case EMult(par1, par2)  => EMult(substitute(par1, env), substitute(par2, env))
      case EDiv(par1, par2)   => EDiv(substitute(par1, env), substitute(par2, env))
      case EPlus(par1, par2)  => EPlus(substitute(par1, env), substitute(par2, env))
      case EMinus(par1, par2) => EMinus(substitute(par1, env), substitute(par2, env))
      case ELt(par1, par2)    => ELt(substitute(par1, env), substitute(par2, env))
      case ELte(par1, par2)   => ELte(substitute(par1, env), substitute(par2, env))
      case EGt(par1, par2)    => EGt(substitute(par1, env), substitute(par2, env))
      case EGte(par1, par2)   => EGte(substitute(par1, env), substitute(par2, env))
      case EEq(par1, par2)    => EEq(substitute(par1, env), substitute(par2, env))
      case ENeq(par1, par2)   => ENeq(substitute(par1, env), substitute(par2, env))
      case EAnd(par1, par2)   => EAnd(substitute(par1, env), substitute(par2, env))
      case EOr(par1, par2)    => EOr(substitute(par1, env), substitute(par2, env))
      case EList(ps, _) =>
        val _ps = for { par <- ps } yield {
          substitute(par, env)
        }
        val fc = (0 /: _ps) { (i, par) =>
          i + par.freeCount
        }
        EList(_ps, fc)
      case ETuple(ps, _) =>
        val _ps = for { par <- ps } yield {
          substitute(par, env)
        }
        val fc = (0 /: _ps) { (i, par) =>
          i + par.freeCount
        }
        ETuple(_ps, fc)
      case ESet(ps, _) =>
        val _ps = for { par <- ps } yield {
          substitute(par, env)
        }
        val fc = (0 /: _ps) { (i, par) =>
          i + par.freeCount
        }
        ESet(_ps, fc)
      case EMap(kvs, _) =>
        val _ps = for { (p1, p2) <- kvs } yield {
          (substitute(p1, env), substitute(p2, env))
        }
        val fc = (0 /: _ps) { case (i, (p1, p2)) => i + p1.freeCount + p2.freeCount }
        EMap(_ps, fc)
      case g @ _ => g
    }

  def rename(term: Par, j: Int): Par =
    Par(
      for (send <- term.sends) yield {
        rename(send, j)
      },
      for (recv <- term.receives) yield {
        rename(recv, j)
      },
      for (eval <- term.evals) yield {
        rename(eval, j)
      },
      for (neu <- term.news) yield {
        rename(neu, j)
      },
      for (exp <- term.exprs) yield {
        rename(exp, j)
      },
      term.matches,
      term.id,
      term.freeCount
    )

  def rename(term: Quote, j: Int): Quote = Quote(rename(term.p, j))

  def rename(term: Channel, j: Int): Channel =
    term match {
      case q @ Quote(_)   => rename(q, j)
      case ChanVar(varue) => ChanVar(rename(varue, j))
    }

  def rename(term: Send, j: Int): Send =
    Send(
      rename(term.chan, j),
      term.data map { par =>
        rename(par, j)
      },
      term.persistent,
      term.freeCount
    )

  def rename(term: Eval, j: Int): Eval =
    Eval(rename(term.channel, j))

  def rename(term: New, j: Int): New =
    New(term.bindCount, rename(term.p, j))

  def rename(term: Receive, j: Int): Receive =
    Receive(
      for ((xs, channel) <- term.binds) yield {
        (xs, rename(channel, j))
      },
      rename(term.body, j),
      term.persistent,
      term.bindCount,
      term.freeCount
    )

  def rename(term: Var, j: Int): Var =
    term match {
      case BoundVar(i) =>
        if (i + j < 0)
          throw new Error(s"Illegal Index [Variable: $term, Level: ${i + j}]")
        else BoundVar(i + j)
      case _ => term
    }

  def rename(term: Expr, j: Int): Expr =
    term match {
      case EVar(v: Var)             => EVar(rename(v, j))
      case ENot(p: Par)             => ENot(rename(p, j))
      case ENeg(p: Par)             => ENeg(rename(p, j))
      case EMult(p1: Par, p2: Par)  => EMult(rename(p1, j), rename(p2, j))
      case EDiv(p1: Par, p2: Par)   => EDiv(rename(p1, j), rename(p2, j))
      case EPlus(p1: Par, p2: Par)  => EPlus(rename(p1, j), rename(p2, j))
      case EMinus(p1: Par, p2: Par) => EMinus(rename(p1, j), rename(p2, j))
      case ELt(p1: Par, p2: Par)    => ELt(rename(p1, j), rename(p2, j))
      case ELte(p1: Par, p2: Par)   => ELte(rename(p1, j), rename(p2, j))
      case EGt(p1: Par, p2: Par)    => EGt(rename(p1, j), rename(p2, j))
      case EGte(p1: Par, p2: Par)   => EGte(rename(p1, j), rename(p2, j))
      case EEq(p1: Par, p2: Par)    => EEq(rename(p1, j), rename(p2, j))
      case ENeq(p1: Par, p2: Par)   => ENeq(rename(p1, j), rename(p2, j))
      case EAnd(p1: Par, p2: Par)   => EAnd(rename(p1, j), rename(p2, j))
      case EOr(p1: Par, p2: Par)    => EOr(rename(p1, j), rename(p2, j))
      case EList(xs, _fc) =>
        EList(for { par <- xs } yield {
          rename(par, j)
        }, _fc)
      case ETuple(xs, _fc) =>
        ETuple(for { par <- xs } yield {
          rename(par, j)
        }, _fc)
      case ESet(xs, _fc) =>
        ESet(for { par <- xs } yield {
          rename(par, j)
        }, _fc)
      case EMap(xs, _fc) =>
        EMap(for { (par0, par1) <- xs } yield {
          (rename(par0, j), rename(par1, j))
        }, _fc)
      case g @ _ => g
    }

}
