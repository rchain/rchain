package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models._

import scala.collection.immutable.BitSet

object implicits {

  // Channel Related
  def apply(c: ChannelInstance): Channel =
    c match {
      case Quote(q) =>
        new Channel(channelInstance = c, freeCount = q.freeCount, locallyFree = q.locallyFree)
      case ChanVar(v) =>
        new Channel(channelInstance = c, freeCount = v.freeCount, locallyFree = v.locallyFree)
    }
  implicit def fromChannelInstance(c: ChannelInstance): Channel                        = apply(c)
  implicit def fromChannel[T](c: T)(implicit toChannel: T => Channel): Option[Channel] = Some(c)

  // Var Related

  def apply(v: VarInstance): Var = v match {
    case b: BoundVar => new Var(varInstance = b, freeCount = 0, locallyFree = BitSet(b.value))
    case FreeVar(_)  => new Var(varInstance = v, freeCount = 1, locallyFree = BitSet())
  }
  implicit def fromVarInstance(v: VarInstance): Var                    = apply(v)
  implicit def fromVar[T](v: T)(implicit toVar: T => Var): Option[Var] = Some(v)

  // Expr Related
  def apply(e: ExprInstance)                 = new Expr(exprInstance = e, freeCount = 0, locallyFree = BitSet())
  implicit def fromGBool(g: GBool): Expr     = apply(g)
  implicit def fromGInt(g: GInt): Expr       = apply(g)
  implicit def fromGString(g: GString): Expr = apply(g)
  implicit def fromGUri(g: GUri): Expr       = apply(g)

  implicit def fromEList(e: EList): EListBody    = EListBody(e)
  implicit def fromETuple(e: ETuple): ETupleBody = ETupleBody(e)
  implicit def fromESet(e: ESet): ESetBody       = ESetBody(e)
  implicit def fromEMap(e: EMap): EMapBody       = EMapBody(e)

  def apply(e: ENot): Expr = {
    val p: Par = e.p.get
    new Expr(exprInstance = ENotBody(e), freeCount = p.freeCount, locallyFree = p.locallyFree)
  }
  implicit def fromENot(e: ENot): Expr = apply(e)

  def apply(e: ENeg): Expr = {
    val p: Par = e.p.get
    new Expr(exprInstance = ENegBody(e), freeCount = p.freeCount, locallyFree = p.locallyFree)
  }
  implicit def fromENeg(e: ENeg): Expr = apply(e)

  def apply(e: EVar): Expr = {
    val v: Var = e.v.get
    new Expr(exprInstance = EVarBody(e), freeCount = v.freeCount, locallyFree = v.locallyFree)
  }
  implicit def fromEVar(e: EVar): Expr = apply(e)

  def apply(e: EMult): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = EMultBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromEMult(e: EMult): Expr = apply(e)

  def apply(e: EDiv): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = EDivBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromEDiv(e: EDiv): Expr = apply(e)

  def apply(e: EPlus): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = EPlusBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromEPlus(e: EPlus): Expr = apply(e)

  def apply(e: EMinus): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = EMinusBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromEMinus(e: EMinus): Expr = apply(e)

  def apply(e: ELt): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = ELtBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromELt(e: ELt): Expr = apply(e)

  def apply(e: ELte): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = ELteBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromELte(e: ELte): Expr = apply(e)

  def apply(e: EGt): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = EGtBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromEGt(e: EGt): Expr = apply(e)

  def apply(e: EGte): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = EGteBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromEGte(e: EGte): Expr = apply(e)

  def apply(e: EEq): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = EEqBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromEEq(e: EEq): Expr = apply(e)

  def apply(e: ENeq): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = ENeqBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromENeq(e: ENeq): Expr = apply(e)

  def apply(e: EAnd): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = EAndBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromEAnd(e: EAnd): Expr = apply(e)

  def apply(e: EOr): Expr = {
    val p1: Par = e.p1.get
    val p2: Par = e.p2.get
    new Expr(exprInstance = EOrBody(e),
             freeCount = p1.freeCount + p2.freeCount,
             locallyFree = p1.locallyFree | p2.locallyFree)
  }
  implicit def fromEOr(e: EOr): Expr = apply(e)

  // Par Related

  def apply(): Par = new Par()
  def apply(s: Send): Par =
    new Par(sends = List(s), freeCount = s.freeCount, locallyFree = s.locallyFree)
  def apply(r: Receive): Par =
    new Par(receives = List(r), freeCount = r.freeCount, locallyFree = r.locallyFree)
  def apply(e: Eval): Par =
    new Par(evals = List(e), freeCount = e.freeCount, locallyFree = e.locallyFree)
  def apply(n: New): Par =
    new Par(news = List(n), freeCount = n.freeCount, locallyFree = n.locallyFree)
  def apply(e: Expr): Par =
    new Par(exprs = List(e), freeCount = e.freeCount, locallyFree = e.locallyFree)
  def apply(m: Match): Par =
    new Par(matches = List(m), freeCount = m.freeCount, locallyFree = m.locallyFree)
  def apply(g: GPrivate): Par =
    new Par(ids = List(g), freeCount = 0, locallyFree = BitSet())

  implicit def fromSend(s: Send): Par                             = apply(s)
  implicit def fromReceive(r: Receive): Par                       = apply(r)
  implicit def fromEval[T](e: T)(implicit toEval: T => Eval): Par = apply(e)
  implicit def fromNew(n: New): Par                               = apply(n)
  implicit def fromExpr[T](e: T)(implicit toExpr: T => Expr): Par = apply(e)
  implicit def fromMatch(m: Match): Par                           = apply(m)
  implicit def fromGPrivate(g: GPrivate): Par                     = apply(g)

  object GPrivate {
    def apply(): GPrivate = new GPrivate(java.util.UUID.randomUUID.toString)
  }

  implicit class ParExtension[T](p: T)(implicit toPar: T => Par) {
    // Convenience prepend methods
    def prepend(s: Send): Par =
      p.copy(sends = Seq(s) ++ p.sends,
             freeCount = p.freeCount + s.freeCount,
             locallyFree = p.locallyFree | s.locallyFree)
    def prepend(r: Receive): Par =
      p.copy(receives = Seq(r) ++ p.receives,
             freeCount = p.freeCount + r.freeCount,
             locallyFree = p.locallyFree | r.locallyFree)
    def prepend(e: Eval): Par =
      p.copy(evals = Seq(e) ++ p.evals,
             freeCount = p.freeCount + e.freeCount,
             locallyFree = p.locallyFree | e.locallyFree)
    def prepend(n: New): Par =
      p.copy(news = Seq(n) ++ p.news,
             freeCount = p.freeCount + n.freeCount,
             locallyFree = p.locallyFree | n.locallyFree)
    def prepend(e: Expr): Par =
      p.copy(exprs = Seq(e) ++ p.exprs,
             freeCount = p.freeCount + e.freeCount,
             locallyFree = p.locallyFree | e.locallyFree)
    def prepend(m: Match): Par =
      p.copy(matches = Seq(m) ++ p.matches,
             freeCount = p.freeCount + m.freeCount,
             locallyFree = p.locallyFree | m.locallyFree)

    def singleEval(): Option[Eval] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.exprs.isEmpty && p.matches.isEmpty) {
        p.evals match {
          case List(single) => Some(single)
          case _            => None
        }
      } else {
        None
      }

    def singleNew(): Option[New] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.evals.isEmpty && p.exprs.isEmpty && p.matches.isEmpty) {
        p.news match {
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
        that.ids ++ p.ids,
        that.freeCount + p.freeCount,
        that.locallyFree | p.locallyFree
      )
  }

  implicit def fromPar[T](p: T)(implicit toPar: T => Par): Option[Par] = Some(p)
}
