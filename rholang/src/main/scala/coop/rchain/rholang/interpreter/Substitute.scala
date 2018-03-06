package coop.rchain.rholang.interpreter

import coop.rchain.rholang.interpreter.Env.DeBruijn

object Substitute {

  def subOrDec(term: Var)(implicit env: Env[Par]): Either[Var, Par] =
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

  def subOrDec(exp: EVar)(implicit env: Env[Par]): Either[EVar, Par] =
    subOrDec(exp.v) match {
      case Left(varue) => Left(EVar(varue))
      case Right(par)  => Right(par)
    }

  def substitute(term: Channel)(implicit env: Env[Par]): Channel =
    term match {
      case Quote(p) => Quote(substitute(p))
      case ChanVar(v) =>
        subOrDec(v) match {
          case Left(_v) => ChanVar(_v)
          case Right(p) => Quote(p)
        }
    }

  def substitute(term: Par)(implicit env: Env[Par]): Par = {

    def subExp(expxs: List[Expr]): Par =
      (expxs :\ Par()) { (expr, par) =>
        expr match {
          case e @ EVar(_) =>
            subOrDec(e) match {
              case Left(_e)    => par.prepend(_e)
              case Right(_par) => _par ++ par
            }
          case e @ _ => par.prepend(substitute(e))
        }
      }

    Par(
      for { s <- term.sends } yield substitute(s),
      for { r <- term.receives } yield substitute(r),
      for { e <- term.evals } yield substitute(e),
      for { n <- term.news } yield substitute(n),
      Nil,
      for { m <- term.matches } yield substitute(m),
      term.id,
      term.freeCount
    ) ++ subExp(term.exprs)
  }

  def substitute(term: Send)(implicit env: Env[Par]): Send =
    Send(
      substitute(term.chan),
      term.data map { par =>
        substitute(par)
      },
      term.persistent,
      term.freeCount
    )

  def substitute(term: Receive)(implicit env: Env[Par]): Receive =
    Receive(
      for { (xs, chan) <- term.binds } yield {
        (xs, substitute(chan))
      },
      substitute(term.body),
      term.persistent,
      term.bindCount,
      term.freeCount
    )

  def substitute(term: Eval)(implicit env: Env[Par]): Eval =
    Eval(substitute(term.channel))

  def substitute(term: New)(implicit env: Env[Par]): New =
    New(term.bindCount, substitute(term.p))

  def substitute(term: Match)(implicit env: Env[Par]): Match =
    Match(
      substitute(term.value),
      for { (_case, par) <- term.cases } yield {
        (_case, substitute(par))
      },
      term.freeCount
    )

  def substitute(exp: Expr)(implicit env: Env[Par]): Expr =
    exp match {
      case ENot(par)          => ENot(substitute(par))
      case ENeg(par)          => ENeg(substitute(par))
      case EMult(par1, par2)  => EMult(substitute(par1), substitute(par2))
      case EDiv(par1, par2)   => EDiv(substitute(par1), substitute(par2))
      case EPlus(par1, par2)  => EPlus(substitute(par1), substitute(par2))
      case EMinus(par1, par2) => EMinus(substitute(par1), substitute(par2))
      case ELt(par1, par2)    => ELt(substitute(par1), substitute(par2))
      case ELte(par1, par2)   => ELte(substitute(par1), substitute(par2))
      case EGt(par1, par2)    => EGt(substitute(par1), substitute(par2))
      case EGte(par1, par2)   => EGte(substitute(par1), substitute(par2))
      case EEq(par1, par2)    => EEq(substitute(par1), substitute(par2))
      case ENeq(par1, par2)   => ENeq(substitute(par1), substitute(par2))
      case EAnd(par1, par2)   => EAnd(substitute(par1), substitute(par2))
      case EOr(par1, par2)    => EOr(substitute(par1), substitute(par2))
      case EList(ps, _) =>
        val _ps = for { par <- ps } yield {
          substitute(par)
        }
        val fc = (0 /: _ps) { (i, par) =>
          i + par.freeCount
        }
        EList(_ps, fc)
      case ETuple(ps, _) =>
        val _ps = for { par <- ps } yield {
          substitute(par)
        }
        val fc = (0 /: _ps) { (i, par) =>
          i + par.freeCount
        }
        ETuple(_ps, fc)
      case ESet(ps, _) =>
        val _ps = for { par <- ps } yield {
          substitute(par)
        }
        val fc = (0 /: _ps) { (i, par) =>
          i + par.freeCount
        }
        ESet(_ps, fc)
      case EMap(kvs, _) =>
        val _ps = for { (p1, p2) <- kvs } yield {
          (substitute(p1), substitute(p2))
        }
        val fc = (0 /: _ps) {
          case (i, (p1, p2)) =>
            i + p1.freeCount + p2.freeCount
        }
        EMap(_ps, fc)
      case g @ _ => g
    }

  def rename(term: Var, j: Int): Var =
    term match {
      case BoundVar(i) =>
        if (i + j < 0)
          throw new Error(s"Illegal Index [Variable: $term, Level: ${i + j}]")
        else BoundVar(i + j)
      case _ => term
    }

  def rename(term: Channel, j: Int): Channel =
    term match {
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
      term.id,
      term.freeCount
    )

  def rename(term: Send, j: Int): Send =
    Send(
      rename(term.chan, j),
      term.data map { par =>
        rename(par, j)
      },
      term.persistent,
      term.freeCount
    )

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

  def rename(term: Eval, j: Int): Eval =
    Eval(rename(term.channel, j))

  def rename(term: New, j: Int): New =
    New(term.bindCount, rename(term.p, j))

  def rename(term: Match, j: Int): Match =
    Match(
      rename(term.value, j),
      for ((_case, par) <- term.cases) yield {
        (_case, rename(par, j))
      },
      term.freeCount
    )

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
