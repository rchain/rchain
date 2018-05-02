package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance.{ChanVar, Quote}
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}

import scala.collection.immutable.BitSet

trait HasLocallyFree[T] {
  def wildcard(source: T): Boolean
  def locallyFree(source: T): BitSet
  def freeCount(source: T): Int
}

object HasLocallyFree {
  def apply[T](implicit ev: HasLocallyFree[T]): HasLocallyFree[T] = ev

  def wildcard[T: HasLocallyFree](source: T): Boolean =
    HasLocallyFree[T].wildcard(source)

  def locallyFree[T: HasLocallyFree](source: T): BitSet =
    HasLocallyFree[T].locallyFree(source)

  def freeCount[T: HasLocallyFree](source: T): Int =
    HasLocallyFree[T].freeCount(source)

}

object HasLocallyFreeInstances {

  implicit val BundleLocallyFree: HasLocallyFree[Bundle] = new HasLocallyFree[Bundle] {
    override def wildcard(source: Bundle): Boolean   = false
    override def freeCount(source: Bundle): Int      = 0
    override def locallyFree(source: Bundle): BitSet = source.body.get.locallyFree
  }

  implicit val SendLocallyFree: HasLocallyFree[Send] = new HasLocallyFree[Send] {
    def wildcard(s: Send)    = s.wildcard
    def freeCount(s: Send)   = s.freeCount
    def locallyFree(s: Send) = s.locallyFree
  }
  implicit val ExprLocallyFree: HasLocallyFree[Expr] = new HasLocallyFree[Expr] {
    def wildcard(e: Expr) =
      e.exprInstance match {
        case GBool(_)                   => false
        case GInt(_)                    => false
        case GString(_)                 => false
        case GUri(_)                    => false
        case EListBody(e)               => e.wildcard
        case ETupleBody(e)              => e.wildcard
        case ESetBody(e)                => e.wildcard
        case EMapBody(e)                => e.wildcard
        case EVarBody(EVar(v))          => VarLocallyFree.wildcard(v.get)
        case ENotBody(ENot(p))          => p.get.wildcard
        case ENegBody(ENeg(p))          => p.get.wildcard
        case EMultBody(EMult(p1, p2))   => p1.get.wildcard || p2.get.wildcard
        case EDivBody(EDiv(p1, p2))     => p1.get.wildcard || p2.get.wildcard
        case EPlusBody(EPlus(p1, p2))   => p1.get.wildcard || p2.get.wildcard
        case EMinusBody(EMinus(p1, p2)) => p1.get.wildcard || p2.get.wildcard
        case ELtBody(ELt(p1, p2))       => p1.get.wildcard || p2.get.wildcard
        case ELteBody(ELte(p1, p2))     => p1.get.wildcard || p2.get.wildcard
        case EGtBody(EGt(p1, p2))       => p1.get.wildcard || p2.get.wildcard
        case EGteBody(EGte(p1, p2))     => p1.get.wildcard || p2.get.wildcard
        case EEqBody(EEq(p1, p2))       => p1.get.wildcard || p2.get.wildcard
        case ENeqBody(ENeq(p1, p2))     => p1.get.wildcard || p2.get.wildcard
        case EAndBody(EAnd(p1, p2))     => p1.get.wildcard || p2.get.wildcard
        case EOrBody(EOr(p1, p2))       => p1.get.wildcard || p2.get.wildcard
        case EMethodBody(e)             => e.wildcard
      }

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
        case EMethodBody(e)             => e.freeCount
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
        case EMethodBody(e)             => e.locallyFree
      }
  }

  implicit val GPrivateLocallyFree: HasLocallyFree[GPrivate] = new HasLocallyFree[GPrivate] {
    def wildcard(g: GPrivate)    = false
    def freeCount(g: GPrivate)   = 0
    def locallyFree(g: GPrivate) = BitSet()
  }

  implicit val ChannelLocallyFree: HasLocallyFree[Channel] = new HasLocallyFree[Channel] {
    def wildcard(c: Channel) =
      c.channelInstance match {
        case Quote(p)   => p.wildcard
        case ChanVar(v) => VarLocallyFree.wildcard(v)
      }

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

  implicit val NewLocallyFree: HasLocallyFree[New] = new HasLocallyFree[New] {
    def wildcard(n: New)    = n.p.get.wildcard
    def freeCount(n: New)   = n.p.get.freeCount
    def locallyFree(n: New) = n.locallyFree
  }

  implicit val EvalLocallyFree: HasLocallyFree[Eval] = new HasLocallyFree[Eval] {
    def wildcard(e: Eval)    = ChannelLocallyFree.wildcard(e.channel.get)
    def freeCount(e: Eval)   = ChannelLocallyFree.freeCount(e.channel.get)
    def locallyFree(e: Eval) = ChannelLocallyFree.locallyFree(e.channel.get)
  }

  implicit val VarLocallyFree: HasLocallyFree[Var] = new HasLocallyFree[Var] {
    def wildcard(v: Var) =
      v.varInstance match {
        case BoundVar(_) => false
        case FreeVar(_)  => false
        case Wildcard(_) => true
      }

    def freeCount(v: Var) =
      v.varInstance match {
        case BoundVar(_) => 0
        case FreeVar(_)  => 1
        case Wildcard(_) => 0
      }

    def locallyFree(v: Var) =
      v.varInstance match {
        case BoundVar(level) => BitSet(level)
        case FreeVar(_)      => BitSet()
        case Wildcard(_)     => BitSet()
      }
  }

  implicit val ReceiveBindLocallyFree: HasLocallyFree[ReceiveBind] =
    new HasLocallyFree[ReceiveBind] {
      def wildcard(rb: ReceiveBind) =
        ChannelLocallyFree.wildcard(rb.source.get)
      def freeCount(rb: ReceiveBind) =
        ChannelLocallyFree.freeCount(rb.source.get)
      def locallyFree(rb: ReceiveBind) =
        ChannelLocallyFree.locallyFree(rb.source.get)
    }
}
