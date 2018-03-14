package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models._

import scala.collection.immutable.BitSet

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

  // Par Related

  def apply(): Par = new Par()
  def apply(s: Send): Par =
    new Par(sends = List(s), freeCount = s.freeCount, locallyFree = s.locallyFree)
  def apply(r: Receive): Par =
    new Par(receives = List(r), freeCount = r.freeCount, locallyFree = r.locallyFree)
  def apply(e: Eval): Par =
    new Par(evals = List(e),
            freeCount = EvalLocallyFree.freeCount(e),
            locallyFree = EvalLocallyFree.locallyFree(e))
  def apply(n: New): Par =
    new Par(news = List(n), freeCount = n.p.get.freeCount, locallyFree = n.locallyFree)
  def apply(e: Expr): Par =
    new Par(exprs = List(e),
            freeCount = ExprLocallyFree.freeCount(e),
            locallyFree = ExprLocallyFree.locallyFree(e))
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
    def apply(): GPrivate          = new GPrivate(java.util.UUID.randomUUID.toString)
    def apply(s: String): GPrivate = new GPrivate(s)
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
             freeCount = p.freeCount + EvalLocallyFree.freeCount(e),
             locallyFree = p.locallyFree | EvalLocallyFree.locallyFree(e))
    def prepend(n: New): Par =
      p.copy(news = Seq(n) ++ p.news,
             freeCount = p.freeCount + n.p.get.freeCount,
             locallyFree = p.locallyFree | n.locallyFree)
    def prepend(e: Expr): Par =
      p.copy(exprs = Seq(e) ++ p.exprs,
             freeCount = p.freeCount + ExprLocallyFree.freeCount(e),
             locallyFree = p.locallyFree | ExprLocallyFree.locallyFree(e))
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

  implicit val SendLocallyFree: HasLocallyFree[Send] = new HasLocallyFree[Send] {
    def freeCount(s: Send)   = s.freeCount
    def locallyFree(s: Send) = s.locallyFree
  }

  implicit val ExprLocallyFree: HasLocallyFree[Expr] = new HasLocallyFree[Expr] {
    def freeCount(e: Expr) =
      e.exprInstance match {
        case GBool(_)                   => 0
        case GInt(_)                    => 0
        case GString(_)                 => 0
        case GUri(_)                    => 0
        case EListBody(e)               => e.freeCount
        case ETupleBody(e)              => e.freeCount
        case ESetBody(e)                => e.freeCount
        case EMapBody(e)                => e.freeCount
        case EVarBody(EVar(v))          => VarLocallyFree.freeCount(v.get)
        case ENotBody(ENot(p))          => p.get.freeCount
        case ENegBody(ENeg(p))          => p.get.freeCount
        case EMultBody(EMult(p1, p2))   => p1.get.freeCount + p2.get.freeCount
        case EDivBody(EDiv(p1, p2))     => p1.get.freeCount + p2.get.freeCount
        case EPlusBody(EPlus(p1, p2))   => p1.get.freeCount + p2.get.freeCount
        case EMinusBody(EMinus(p1, p2)) => p1.get.freeCount + p2.get.freeCount
        case ELtBody(ELt(p1, p2))       => p1.get.freeCount + p2.get.freeCount
        case ELteBody(ELte(p1, p2))     => p1.get.freeCount + p2.get.freeCount
        case EGtBody(EGt(p1, p2))       => p1.get.freeCount + p2.get.freeCount
        case EGteBody(EGte(p1, p2))     => p1.get.freeCount + p2.get.freeCount
        case EEqBody(EEq(p1, p2))       => p1.get.freeCount + p2.get.freeCount
        case ENeqBody(ENeq(p1, p2))     => p1.get.freeCount + p2.get.freeCount
        case EAndBody(EAnd(p1, p2))     => p1.get.freeCount + p2.get.freeCount
        case EOrBody(EOr(p1, p2))       => p1.get.freeCount + p2.get.freeCount
      }

    def locallyFree(e: Expr) =
      e.exprInstance match {
        case GBool(_)      => BitSet()
        case GInt(_)       => BitSet()
        case GString(_)    => BitSet()
        case GUri(_)       => BitSet()
        case EListBody(e)  => e.locallyFree
        case ETupleBody(e) => e.locallyFree
        case ESetBody(e)   => e.locallyFree
        case EMapBody(e)   => e.locallyFree
        case EVarBody(EVar(v)) => {
          VarLocallyFree.locallyFree(v.get)
        }
        case ENotBody(ENot(p))          => p.get.locallyFree
        case ENegBody(ENeg(p))          => p.get.locallyFree
        case EMultBody(EMult(p1, p2))   => p1.get.locallyFree | p2.get.locallyFree
        case EDivBody(EDiv(p1, p2))     => p1.get.locallyFree | p2.get.locallyFree
        case EPlusBody(EPlus(p1, p2))   => p1.get.locallyFree | p2.get.locallyFree
        case EMinusBody(EMinus(p1, p2)) => p1.get.locallyFree | p2.get.locallyFree
        case ELtBody(ELt(p1, p2))       => p1.get.locallyFree | p2.get.locallyFree
        case ELteBody(ELte(p1, p2))     => p1.get.locallyFree | p2.get.locallyFree
        case EGtBody(EGt(p1, p2))       => p1.get.locallyFree | p2.get.locallyFree
        case EGteBody(EGte(p1, p2))     => p1.get.locallyFree | p2.get.locallyFree
        case EEqBody(EEq(p1, p2))       => p1.get.locallyFree | p2.get.locallyFree
        case ENeqBody(ENeq(p1, p2))     => p1.get.locallyFree | p2.get.locallyFree
        case EAndBody(EAnd(p1, p2))     => p1.get.locallyFree | p2.get.locallyFree
        case EOrBody(EOr(p1, p2))       => p1.get.locallyFree | p2.get.locallyFree
      }
  }

  implicit val ChannelLocallyFree: HasLocallyFree[Channel] = new HasLocallyFree[Channel] {
    def freeCount(c: Channel) =
      c.channelInstance match {
        case Quote(p)   => p.freeCount
        case ChanVar(v) => VarLocallyFree.freeCount(v)
      }

    def locallyFree(c: Channel) =
      c.channelInstance match {
        case Quote(p)   => p.locallyFree
        case ChanVar(v) => VarLocallyFree.locallyFree(v)
      }
  }

  implicit val EvalLocallyFree: HasLocallyFree[Eval] = new HasLocallyFree[Eval] {
    def freeCount(e: Eval)   = ChannelLocallyFree.freeCount(e.channel.get)
    def locallyFree(e: Eval) = ChannelLocallyFree.locallyFree(e.channel.get)
  }

  implicit val VarLocallyFree: HasLocallyFree[Var] = new HasLocallyFree[Var] {
    def freeCount(v: Var) =
      v.varInstance match {
        case BoundVar(_) => 0
        case FreeVar(_)  => 1
        case Wildcard(_) => 1
      }

    def locallyFree(v: Var) =
      v.varInstance match {
        case BoundVar(level) => BitSet(level)
        case FreeVar(_)      => BitSet()
        case Wildcard(_)     => BitSet()
      }
  }
}
