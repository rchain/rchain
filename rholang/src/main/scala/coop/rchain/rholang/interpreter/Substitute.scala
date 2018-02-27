package coop.rchain.rholang.interpreter

import Env.DeBruijn

sealed trait Substitute

object Substitute {

  def substitute(term: Par, env: Env): Par =
    Par(
      for { send <- term.sends } yield {
        substitute(send, env)
      },
      for { recv <- term.receives } yield {
        substitute(recv, env)
      },
      for { eval <- term.evals } yield {
        substitute(eval, env)
      },
      for { neu <- term.news } yield {
        substitute(neu, env)
      },
      term.exprs, /*for { exp <- term.exprs } yield {
        case e @ EVar(_) =>
          substitute(e, env) match {
            case Left(varue) => EVar(varue)
            case Right(Left(quote)) =>
              throw new Error(s"Channel must be term in process expression: $quote")
            case Right(Right(par)) => return term.merge(par)
          }
        case _ => substitute(exp, env)
      },*/
      term.id
    )

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
      term.id
    )

  def substitute(term: Quote, env: Env): Quote = Quote(substitute(term.p, env))

  def rename(term: Quote, j: Int): Quote = Quote(rename(term.p, j))

  def substitute(term: Channel, env: Env): Either[Channel, Par] =
    term match {
      case q @ Quote(_) => Left(substitute(q, env))
      case ChanVar(b @ _) =>
        substitute(b, env) match {
          case Right(Left(quote)) => Left(quote)
          case Right(Right(par))  => Right(par)
          case Left(c @ _)        => Left(ChanVar(c))
        }
    }

  def rename(term: Channel, j: Int): Channel =
    term match {
      case q @ Quote(_)   => rename(q, j)
      case ChanVar(varue) => ChanVar(rename(varue, j))
    }

  def substitute(term: Send, env: Env): Send =
    substitute(term.chan, env) match {
      case Left(chan) =>
        Send(
          chan,
          term.data map { par =>
            substitute(par, env)
          },
          term.persistent
        )
      case Right(par) => throw new Error(s"Cannot send on expression: $par")
    }

  def rename(term: Send, j: Int): Send =
    Send(
      rename(term.chan, j),
      term.data map { par =>
        rename(par, j)
      },
      term.persistent
    )

  def substitute(term: Eval, env: Env): Eval =
    substitute(term.channel, env) match {
      case Left(_channel) => Eval(_channel)
      case Right(par)     => throw new Error(s"Cannot dereference process: $par")
    }

  def rename(term: Eval, j: Int): Eval =
    Eval(rename(term.channel, j))

  def substitute(term: New, env: Env): New =
    New(term.count, substitute(term.p, env))

  def rename(term: New, j: Int): New =
    New(term.count, rename(term.p, j))

  def substitute(term: Receive, env: Env): Receive =
    Receive(
      for { (xs, chan) <- term.binds } yield {
        substitute(chan, env) match {
          case Left(_chan) => (xs, _chan)
          case Right(par)  => throw new Error(s"Cannot receive on expression: $par")
        }
      },
      substitute(term.body, env),
      term.persistent,
      term.count
    )

  def rename(term: Receive, j: Int): Receive =
    Receive(
      for ((xs, channel) <- term.binds) yield {
        (xs, rename(channel, j))
      },
      rename(term.body, j),
      term.persistent,
      term.count
    )

  def substitute(term: Var, env: Env): Either[Var, Data] =
    term match {
      case BoundVar(level) =>
        env.get(level) match {
          case Some(Left(quote)) => Right(Left(rename(quote, env.level)))
          case Some(Right(par))  => Right(Right(rename(par, env.level)))
          case None =>
            if (level - env.level < 0) Left(BoundVar(0))
            else Left(BoundVar(level - env.level))
        }
      case _ => Left(term)
    }

  def rename(term: Var, j: Int): Var =
    term match {
      case BoundVar(i) =>
        if (i + j < 0) BoundVar(0)
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
    }

  def rename(exp: Ground, j: Int): Ground =
    exp match {
      case EList(xs) =>
        EList(for { par <- xs } yield {
          rename(par, j)
        })
      case ETuple(xs) =>
        ETuple(for { par <- xs } yield {
          rename(par, j)
        })
      case ESet(xs) =>
        ESet(for { par <- xs } yield {
          rename(par, j)
        })
      case EMap(xs) =>
        EMap(for { (par0, par1) <- xs } yield {
          (rename(par0, j), rename(par1, j))
        })
      case g @ _ => g
    }

  def substitute(exp: EVar, env: Env): Either[Var, Data] =
    substitute(exp.v, env)

  def substitute(exp: Expr, env: Env): Expr =
    exp match {
      case ENot(p)        => ENot(substitute(p, env))
      case ENeg(p)        => ENeg(substitute(p, env))
      case EMult(p1, p2)  => EMult(substitute(p1, env), substitute(p2, env))
      case EDiv(p1, p2)   => EDiv(substitute(p1, env), substitute(p2, env))
      case EPlus(p1, p2)  => EPlus(substitute(p1, env), substitute(p2, env))
      case EMinus(p1, p2) => EMinus(substitute(p1, env), substitute(p2, env))
      case ELt(p1, p2)    => ELt(substitute(p1, env), substitute(p2, env))
      case ELte(p1, p2)   => ELte(substitute(p1, env), substitute(p2, env))
      case EGt(p1, p2)    => EGt(substitute(p1, env), substitute(p2, env))
      case EGte(p1, p2)   => EGte(substitute(p1, env), substitute(p2, env))
      case EEq(p1, p2)    => EEq(substitute(p1, env), substitute(p2, env))
      case ENeq(p1, p2)   => ENeq(substitute(p1, env), substitute(p2, env))
      case EAnd(p1, p2)   => EAnd(substitute(p1, env), substitute(p2, env))
      case EOr(p1, p2)    => EOr(substitute(p1, env), substitute(p2, env))
    }

  def substitute(exp: Ground, env: Env): Ground =
    exp match {
      case EList(ps) =>
        EList(for { par <- ps } yield {
          substitute(par, env)
        })
      case ETuple(ps) =>
        ETuple(for { par <- ps } yield {
          substitute(par, env)
        })
      case ESet(ps) =>
        ESet(for { par <- ps } yield {
          substitute(par, env)
        })
      case EMap(kvs) =>
        EMap(for { (par0, par1) <- kvs } yield {
          (substitute(par0, env), substitute(par1, env))
        })
      case g @ _ => g
    }
}
