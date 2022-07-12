package coop.rchain.rholang.interpreter.matcher

import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance.EVarBody
import coop.rchain.models.{Connective, ConnectiveBody, EVar, Par}
import coop.rchain.models.Var.VarInstance.{FreeVar, Wildcard}
import coop.rchain.rholang.interpreter.matcher.ParSpatialMatcherUtils.noFrees

private[matcher] final case class ParCount(
    sends: Int = 0,
    receives: Int = 0,
    news: Int = 0,
    exprs: Int = 0,
    matches: Int = 0,
    unforgeables: Int = 0,
    bundles: Int = 0
) {

  def min(other: ParCount): ParCount = binOp(math.min, other)

  def max(other: ParCount): ParCount = binOp(math.max, other)

  def +(other: ParCount): ParCount = binOp(saturatingAdd, other)

  def binOp(op: (Int, Int) => Int, other: ParCount): ParCount =
    ParCount(
      sends = op(sends, other.sends),
      receives = op(receives, other.receives),
      news = op(news, other.news),
      exprs = op(exprs, other.exprs),
      matches = op(matches, other.matches),
      unforgeables = op(unforgeables, other.unforgeables),
      bundles = op(bundles, other.bundles)
    )

  // Only saturates going from positive to negative
  def saturatingAdd(l: Int, r: Int): Int = {
    val res = l + r
    (res | -(if (res < l) 1 else 0)) & ~Int.MinValue
  }

}

private[matcher] object ParCount {

  def apply(par: Par): ParCount =
    ParCount(
      sends = par.sends.size,
      receives = par.receives.size,
      news = par.news.size,
      matches = par.matches.size,
      exprs = par.exprs.size,
      unforgeables = par.unforgeables.size,
      bundles = par.bundles.size
    )

  def max: ParCount =
    ParCount(
      sends = Int.MaxValue,
      receives = Int.MaxValue,
      news = Int.MaxValue,
      matches = Int.MaxValue,
      exprs = Int.MaxValue,
      unforgeables = Int.MaxValue,
      bundles = Int.MaxValue
    )

  def minMax(par: Par): (ParCount, ParCount) = {
    val pc = ParCount(noFrees(par))
    val wildcard: Boolean = par.exprs.exists { expr =>
      expr.exprInstance match {
        case EVarBody(EVar(v)) =>
          v.varInstance match {
            case Wildcard(_) => true
            case FreeVar(_)  => true
            case _           => false
          }
        case _ => false
      }
    }
    val minInit = pc
    val maxInit = if (wildcard) ParCount.max else pc

    par.connectives.foldLeft((minInit, maxInit)) {
      case ((min, max), con) =>
        val (cmin, cmax) = minMax(con)
        (min + cmin, max + cmax)
    }
  }

  def minMax(con: Connective): (ParCount, ParCount) =
    con.connectiveInstance match {
      case ConnAndBody(ConnectiveBody(ps)) =>
        val pMinMax = ps.map(minMax)
        (pMinMax.foldLeft(ParCount())(_ max _._1), pMinMax.foldLeft(ParCount.max)(_ min _._2))
      case ConnOrBody(ConnectiveBody(ps)) =>
        val pMinMax = ps.map(minMax)
        (pMinMax.foldLeft(ParCount.max)(_ min _._1), pMinMax.foldLeft(ParCount())(_ max _._2))
      case ConnNotBody(_) =>
        (ParCount(), ParCount.max)
      case ConnectiveInstance.Empty =>
        (ParCount(), ParCount())
      case _: VarRefBody =>
        // Variable references should be substituted before going into the matcher.
        // This should never happen.
        (ParCount(), ParCount())
      case _: ConnBool      => (ParCount(exprs = 1), ParCount(exprs = 1))
      case _: ConnInt       => (ParCount(exprs = 1), ParCount(exprs = 1))
      case _: ConnBigInt    => (ParCount(exprs = 1), ParCount(exprs = 1))
      case _: ConnString    => (ParCount(exprs = 1), ParCount(exprs = 1))
      case _: ConnUri       => (ParCount(exprs = 1), ParCount(exprs = 1))
      case _: ConnByteArray => (ParCount(exprs = 1), ParCount(exprs = 1))
    }

}
