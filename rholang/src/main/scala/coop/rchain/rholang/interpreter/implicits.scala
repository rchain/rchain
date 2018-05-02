package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models._
import HasLocallyFreeInstances._

import scala.collection.immutable.{BitSet, Vector}

object implicits {

  // Channel Related
  def apply(c: ChannelInstance): Channel                                               = new Channel(channelInstance = c)
  implicit def fromChannelInstance(c: ChannelInstance): Channel                        = apply(c)
  implicit def fromChannel[T](c: T)(implicit toChannel: T => Channel): Option[Channel] = Some(c)

  // Var Related
  def apply(v: VarInstance): Var                                       = new Var(v)
  implicit def fromVarInstance(v: VarInstance): Var                    = apply(v)
  implicit def fromVar[T](v: T)(implicit toVar: T => Var): Option[Var] = Some(v)

  // Expr Related
  def apply(e: ExprInstance)                 = new Expr(exprInstance = e)
  implicit def fromGBool(g: GBool): Expr     = apply(g)
  implicit def fromGInt(g: GInt): Expr       = apply(g)
  implicit def fromGString(g: GString): Expr = apply(g)
  implicit def fromGUri(g: GUri): Expr       = apply(g)

  def apply(e: EList): Expr =
    new Expr(exprInstance = EListBody(e))
  implicit def fromEList(e: EList): Expr = apply(e)

  def apply(e: ETuple): Expr =
    new Expr(exprInstance = ETupleBody(e))
  implicit def fromEList(e: ETuple): Expr = apply(e)

  def apply(e: ESet): Expr =
    new Expr(exprInstance = ESetBody(e))
  implicit def fromESet(e: ESet): Expr = apply(e)

  def apply(e: EMap): Expr =
    new Expr(exprInstance = EMapBody(e))
  implicit def fromEMap(e: EMap): Expr = apply(e)

  def apply(e: ENot): Expr =
    new Expr(exprInstance = ENotBody(e))
  implicit def fromENot(e: ENot): Expr = apply(e)

  def apply(e: ENeg): Expr =
    new Expr(exprInstance = ENegBody(e))
  implicit def fromENeg(e: ENeg): Expr = apply(e)

  def apply(e: EVar): Expr =
    new Expr(exprInstance = EVarBody(e))
  implicit def fromEVar(e: EVar): Expr = apply(e)

  def apply(e: EMult): Expr =
    new Expr(exprInstance = EMultBody(e))
  implicit def fromEMult(e: EMult): Expr = apply(e)

  def apply(e: EDiv): Expr =
    new Expr(exprInstance = EDivBody(e))
  implicit def fromEDiv(e: EDiv): Expr = apply(e)

  def apply(e: EPlus): Expr =
    new Expr(exprInstance = EPlusBody(e))
  implicit def fromEPlus(e: EPlus): Expr = apply(e)

  def apply(e: EMinus): Expr =
    new Expr(exprInstance = EMinusBody(e))
  implicit def fromEMinus(e: EMinus): Expr = apply(e)

  def apply(e: ELt): Expr =
    new Expr(exprInstance = ELtBody(e))
  implicit def fromELt(e: ELt): Expr = apply(e)

  def apply(e: ELte): Expr =
    new Expr(exprInstance = ELteBody(e))
  implicit def fromELte(e: ELte): Expr = apply(e)

  def apply(e: EGt): Expr =
    new Expr(exprInstance = EGtBody(e))
  implicit def fromEGt(e: EGt): Expr = apply(e)

  def apply(e: EGte): Expr =
    new Expr(exprInstance = EGteBody(e))
  implicit def fromEGte(e: EGte): Expr = apply(e)

  def apply(e: EEq): Expr =
    new Expr(exprInstance = EEqBody(e))
  implicit def fromEEq(e: EEq): Expr = apply(e)

  def apply(e: ENeq): Expr =
    new Expr(exprInstance = ENeqBody(e))
  implicit def fromENeq(e: ENeq): Expr = apply(e)

  def apply(e: EAnd): Expr =
    new Expr(exprInstance = EAndBody(e))
  implicit def fromEAnd(e: EAnd): Expr = apply(e)

  def apply(e: EOr): Expr =
    new Expr(exprInstance = EOrBody(e))
  implicit def fromEOr(e: EOr): Expr = apply(e)

  def apply(e: EMethod): Expr =
    new Expr(exprInstance = EMethodBody(e))
  implicit def fromEMethod(e: EMethod): Expr = apply(e)

  // Par Related
  def apply(): Par = new Par()
  def apply(s: Send): Par =
    new Par(sends = List(s),
            freeCount = s.freeCount,
            locallyFree = s.locallyFree,
            wildcard = s.wildcard)
  def apply(r: Receive): Par =
    new Par(receives = List(r),
            freeCount = r.freeCount,
            locallyFree = r.locallyFree,
            wildcard = r.wildcard)
  def apply(e: Eval): Par =
    new Par(evals = List(e),
            freeCount = EvalLocallyFree.freeCount(e),
            locallyFree = EvalLocallyFree.locallyFree(e),
            wildcard = EvalLocallyFree.wildcard(e))
  def apply(n: New): Par =
    new Par(news = List(n),
            freeCount = NewLocallyFree.freeCount(n),
            locallyFree = NewLocallyFree.locallyFree(n),
            wildcard = NewLocallyFree.wildcard(n))
  def apply(e: Expr): Par =
    new Par(exprs = List(e),
            freeCount = ExprLocallyFree.freeCount(e),
            locallyFree = ExprLocallyFree.locallyFree(e),
            wildcard = ExprLocallyFree.wildcard(e))
  def apply(m: Match): Par =
    new Par(matches = List(m),
            freeCount = m.freeCount,
            locallyFree = m.locallyFree,
            wildcard = m.wildcard)
  def apply(g: GPrivate): Par =
    new Par(ids = List(g), freeCount = 0, locallyFree = BitSet(), wildcard = false)

  def apply(b: Bundle): Par =
    new Par(
      bundles = Seq(b),
      freeCount = 0,
      locallyFree = b.body.get.locallyFree,
      wildcard = false
    )

  implicit def fromSend(s: Send): Par                             = apply(s)
  implicit def fromReceive(r: Receive): Par                       = apply(r)
  implicit def fromEval[T](e: T)(implicit toEval: T => Eval): Par = apply(e)
  implicit def fromNew(n: New): Par                               = apply(n)
  implicit def fromExpr[T](e: T)(implicit toExpr: T => Expr): Par = apply(e)
  implicit def fromMatch(m: Match): Par                           = apply(m)
  implicit def fromGPrivate(g: GPrivate): Par                     = apply(g)
  implicit def fromBundle(b: Bundle): Par                         = apply(b)

  object VectorPar {
    def apply(): Par = new Par(
      sends = Vector.empty[Send],
      receives = Vector.empty[Receive],
      evals = Vector.empty[Eval],
      news = Vector.empty[New],
      exprs = Vector.empty[Expr],
      matches = Vector.empty[Match],
      ids = Vector.empty[GPrivate],
    )
  }

  object GPrivate {
    def apply(): GPrivate          = new GPrivate(java.util.UUID.randomUUID.toString)
    def apply(s: String): GPrivate = new GPrivate(s)
  }

  implicit class ParExtension[T](p: T)(implicit toPar: T => Par) {
    // Convenience prepend methods
    def prepend(s: Send): Par =
      p.copy(sends = Seq(s) ++ p.sends,
             freeCount = p.freeCount + s.freeCount,
             locallyFree = p.locallyFree | s.locallyFree,
             wildcard = p.wildcard || s.wildcard)
    def prepend(r: Receive): Par =
      p.copy(receives = Seq(r) ++ p.receives,
             freeCount = p.freeCount + r.freeCount,
             locallyFree = p.locallyFree | r.locallyFree,
             wildcard = p.wildcard || r.wildcard)
    def prepend(e: Eval): Par =
      p.copy(
        evals = Seq(e) ++ p.evals,
        freeCount = p.freeCount + EvalLocallyFree.freeCount(e),
        locallyFree = p.locallyFree | EvalLocallyFree.locallyFree(e),
        wildcard = p.wildcard || EvalLocallyFree.wildcard(e)
      )
    def prepend(n: New): Par =
      p.copy(
        news = Seq(n) ++ p.news,
        freeCount = p.freeCount + NewLocallyFree.freeCount(n),
        locallyFree = p.locallyFree | NewLocallyFree.locallyFree(n),
        wildcard = p.wildcard || NewLocallyFree.wildcard(n)
      )
    def prepend(e: Expr): Par =
      p.copy(
        exprs = Seq(e) ++ p.exprs,
        freeCount = p.freeCount + ExprLocallyFree.freeCount(e),
        locallyFree = p.locallyFree | ExprLocallyFree.locallyFree(e),
        wildcard = p.wildcard || ExprLocallyFree.wildcard(e)
      )
    def prepend(b: Bundle): Par =
      p.copy(
        bundles = Seq(b) ++ p.bundles,
        locallyFree = b.body.get.locallyFree | p.locallyFree
      )
    def prepend(m: Match): Par =
      p.copy(matches = Seq(m) ++ p.matches,
             freeCount = p.freeCount + m.freeCount,
             locallyFree = p.locallyFree | m.locallyFree,
             wildcard = p.wildcard || m.wildcard)

    def singleEval(): Option[Eval] =
      if (p.bundles.isEmpty && p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.exprs.isEmpty && p.matches.isEmpty) {
        p.evals match {
          case List(single) => Some(single)
          case _            => None
        }
      } else {
        None
      }

    def singleNew(): Option[New] =
      if (p.bundles.isEmpty && p.sends.isEmpty && p.receives.isEmpty && p.evals.isEmpty && p.exprs.isEmpty && p.matches.isEmpty) {
        p.news match {
          case List(single) => Some(single)
          case _            => None
        }
      } else {
        None
      }

    def singleBundle(): Option[Bundle] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.evals.isEmpty && p.news.isEmpty && p.exprs.isEmpty && p.matches.isEmpty) {
        p.bundles match {
          case List(single) => Some(single)
          case _            => None
        }
      } else {
        None
      }

    def ++(that: Par) =
      Par(
        that.sends ++ p.sends,
        that.receives ++ p.receives,
        that.evals ++ p.evals,
        that.news ++ p.news,
        that.exprs ++ p.exprs,
        that.matches ++ p.matches,
        that.bundles ++ p.bundles,
        that.ids ++ p.ids,
        that.freeCount + p.freeCount,
        that.locallyFree | p.locallyFree,
        that.wildcard || p.wildcard
      )
  }

  implicit def fromPar[T](p: T)(implicit toPar: T => Par): Option[Par] = Some(p)

}
