package coop.rchain.rholang.interpreter

import coop.rchain.models.Channel.ChannelInstance
import coop.rchain.models.Channel.ChannelInstance._
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models._

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
  def apply(e: ExprInstance)                     = new Expr(exprInstance = e)
  implicit def fromExprInstance(e: ExprInstance) = apply(e)

  implicit def fromGBool(g: GBool): Expr          = apply(g)
  implicit def fromGInt(g: GInt): Expr            = apply(g)
  implicit def fromGString(g: GString): Expr      = apply(g)
  implicit def fromGUri(g: GUri): Expr            = apply(g)
  implicit def fromByteArray(g: GByteArray): Expr = apply(g)

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
    new Par(sends = Vector(s), locallyFree = s.locallyFree, connectiveUsed = s.connectiveUsed)
  def apply(r: Receive): Par =
    new Par(receives = Vector(r), locallyFree = r.locallyFree, connectiveUsed = r.connectiveUsed)
  def apply(n: New): Par =
    new Par(news = Vector(n),
            locallyFree = NewLocallyFree.locallyFree(n),
            connectiveUsed = NewLocallyFree.connectiveUsed(n))
  def apply(e: Expr): Par =
    new Par(exprs = Vector(e),
            locallyFree = ExprLocallyFree.locallyFree(e),
            connectiveUsed = ExprLocallyFree.connectiveUsed(e))
  def apply(m: Match): Par =
    new Par(matches = Vector(m), locallyFree = m.locallyFree, connectiveUsed = m.connectiveUsed)
  def apply(g: GPrivate): Par =
    new Par(ids = Vector(g), locallyFree = BitSet(), connectiveUsed = false)

  def apply(b: Bundle): Par =
    new Par(
      bundles = Vector(b),
      locallyFree = b.body.get.locallyFree,
      connectiveUsed = false
    )

  def apply(c: Connective): Par =
    new Par(connectives = Vector(c), connectiveUsed = true)

  implicit def fromSend(s: Send): Par                             = apply(s)
  implicit def fromReceive(r: Receive): Par                       = apply(r)
  implicit def fromNew(n: New): Par                               = apply(n)
  implicit def fromExpr[T](e: T)(implicit toExpr: T => Expr): Par = apply(e)
  implicit def fromMatch(m: Match): Par                           = apply(m)
  implicit def fromGPrivate(g: GPrivate): Par                     = apply(g)
  implicit def fromBundle(b: Bundle): Par                         = apply(b)
  implicit def fromConnective(c: Connective): Par                 = apply(c)

  object VectorPar {
    def apply(): Par = new Par(
      sends = Vector.empty[Send],
      receives = Vector.empty[Receive],
      news = Vector.empty[New],
      exprs = Vector.empty[Expr],
      matches = Vector.empty[Match],
      ids = Vector.empty[GPrivate],
      bundles = Vector.empty[Bundle],
      connectives = Vector.empty[Connective],
    )
  }

  object GPrivate {
    def apply(): GPrivate          = new GPrivate(java.util.UUID.randomUUID.toString)
    def apply(s: String): GPrivate = new GPrivate(s)
  }

  implicit class ParExtension[T](p: T)(implicit toPar: T => Par) {
    // Convenience prepend methods
    def prepend(s: Send): Par =
      p.copy(sends = s +: p.sends,
             locallyFree = p.locallyFree | s.locallyFree,
             connectiveUsed = p.connectiveUsed || s.connectiveUsed)
    def prepend(r: Receive): Par =
      p.copy(
        receives = r +: p.receives,
        locallyFree = p.locallyFree | r.locallyFree,
        connectiveUsed = p.connectiveUsed || r.connectiveUsed
      )
    def prepend(n: New): Par =
      p.copy(
        news = n +: p.news,
        locallyFree = p.locallyFree | NewLocallyFree.locallyFree(n),
        connectiveUsed = p.connectiveUsed || NewLocallyFree.connectiveUsed(n)
      )
    def prepend(e: Expr): Par =
      p.copy(
        exprs = e +: p.exprs,
        locallyFree = p.locallyFree | ExprLocallyFree.locallyFree(e),
        connectiveUsed = p.connectiveUsed || ExprLocallyFree.connectiveUsed(e)
      )
    def prepend(m: Match): Par =
      p.copy(matches = m +: p.matches,
             locallyFree = p.locallyFree | m.locallyFree,
             connectiveUsed = p.connectiveUsed || m.connectiveUsed)
    def prepend(b: Bundle): Par =
      p.copy(
        bundles = b +: p.bundles,
        locallyFree = b.body.get.locallyFree | p.locallyFree
      )
    def prepend(c: Connective): Par =
      p.copy(
        connectives = c +: p.connectives,
        connectiveUsed = true
      )

    def singleEval(): Option[Channel] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.matches.isEmpty && p.ids.isEmpty && p.bundles.isEmpty && p.connectives.isEmpty) {
        p.exprs match {
          case Seq(Expr(EEvalBody(c))) => Some(c)
          case _                       => None
        }
      } else {
        None
      }

    def singleExpr(): Option[Expr] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.matches.isEmpty && p.bundles.isEmpty) {
        p.exprs match {
          case Seq(single) => Some(single)
          case _           => None
        }
      } else {
        None
      }

    def singleNew(): Option[New] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.exprs.isEmpty && p.matches.isEmpty && p.ids.isEmpty && p.bundles.isEmpty && p.connectives.isEmpty) {
        p.news match {
          case Seq(single) => Some(single)
          case _           => None
        }
      } else {
        None
      }

    def singleBundle(): Option[Bundle] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.exprs.isEmpty && p.matches.isEmpty && p.ids.isEmpty && p.connectives.isEmpty) {
        p.bundles.toList match {
          case Seq(single) => Some(single)
          case _           => None
        }
      } else {
        None
      }

    def ++(that: Par) =
      Par(
        that.sends ++ p.sends,
        that.receives ++ p.receives,
        that.news ++ p.news,
        that.exprs ++ p.exprs,
        that.matches ++ p.matches,
        that.ids ++ p.ids,
        that.bundles ++ p.bundles,
        that.connectives ++ p.connectives,
        that.locallyFree | p.locallyFree,
        that.connectiveUsed || p.connectiveUsed
      )
  }

  implicit def fromPar[T](p: T)(implicit toPar: T => Par): Option[Par] = Some(p)

  implicit val ParLocallyFree: HasLocallyFree[Par] = new HasLocallyFree[Par] {
    def connectiveUsed(p: Par) = p.connectiveUsed
    def locallyFree(p: Par)    = p.locallyFree
  }

  implicit val BundleLocallyFree: HasLocallyFree[Bundle] = new HasLocallyFree[Bundle] {
    override def connectiveUsed(source: Bundle): Boolean = false
    override def locallyFree(source: Bundle): BitSet     = source.body.get.locallyFree
  }

  implicit val SendLocallyFree: HasLocallyFree[Send] = new HasLocallyFree[Send] {
    def connectiveUsed(s: Send) = s.connectiveUsed
    def locallyFree(s: Send)    = s.locallyFree
  }
  implicit val ExprLocallyFree: HasLocallyFree[Expr] = new HasLocallyFree[Expr] {
    def connectiveUsed(e: Expr) =
      e.exprInstance match {
        case GBool(_)                   => false
        case GInt(_)                    => false
        case GString(_)                 => false
        case GUri(_)                    => false
        case GByteArray(_)              => false
        case EListBody(e)               => e.connectiveUsed
        case ETupleBody(e)              => e.connectiveUsed
        case ESetBody(e)                => e.connectiveUsed
        case EMapBody(e)                => e.connectiveUsed
        case EVarBody(EVar(v))          => VarLocallyFree.connectiveUsed(v.get)
        case EEvalBody(chan)            => ChannelLocallyFree.connectiveUsed(chan)
        case ENotBody(ENot(p))          => p.get.connectiveUsed
        case ENegBody(ENeg(p))          => p.get.connectiveUsed
        case EMultBody(EMult(p1, p2))   => p1.get.connectiveUsed || p2.get.connectiveUsed
        case EDivBody(EDiv(p1, p2))     => p1.get.connectiveUsed || p2.get.connectiveUsed
        case EPlusBody(EPlus(p1, p2))   => p1.get.connectiveUsed || p2.get.connectiveUsed
        case EMinusBody(EMinus(p1, p2)) => p1.get.connectiveUsed || p2.get.connectiveUsed
        case ELtBody(ELt(p1, p2))       => p1.get.connectiveUsed || p2.get.connectiveUsed
        case ELteBody(ELte(p1, p2))     => p1.get.connectiveUsed || p2.get.connectiveUsed
        case EGtBody(EGt(p1, p2))       => p1.get.connectiveUsed || p2.get.connectiveUsed
        case EGteBody(EGte(p1, p2))     => p1.get.connectiveUsed || p2.get.connectiveUsed
        case EEqBody(EEq(p1, p2))       => p1.get.connectiveUsed || p2.get.connectiveUsed
        case ENeqBody(ENeq(p1, p2))     => p1.get.connectiveUsed || p2.get.connectiveUsed
        case EAndBody(EAnd(p1, p2))     => p1.get.connectiveUsed || p2.get.connectiveUsed
        case EOrBody(EOr(p1, p2))       => p1.get.connectiveUsed || p2.get.connectiveUsed
        case EMethodBody(e)             => e.connectiveUsed
      }

    def locallyFree(e: Expr) =
      e.exprInstance match {
        case GBool(_)                   => BitSet()
        case GInt(_)                    => BitSet()
        case GString(_)                 => BitSet()
        case GUri(_)                    => BitSet()
        case GByteArray(_)              => BitSet()
        case EListBody(e)               => e.locallyFree
        case ETupleBody(e)              => e.locallyFree
        case ESetBody(e)                => e.locallyFree
        case EMapBody(e)                => e.locallyFree
        case EVarBody(EVar(v))          => VarLocallyFree.locallyFree(v.get)
        case EEvalBody(chan)            => ChannelLocallyFree.locallyFree(chan)
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
    def connectiveUsed(g: GPrivate) = false
    def locallyFree(g: GPrivate)    = BitSet()
  }

  implicit val ChannelLocallyFree: HasLocallyFree[Channel] = new HasLocallyFree[Channel] {
    def connectiveUsed(c: Channel) =
      c.channelInstance match {
        case Quote(p)   => p.connectiveUsed
        case ChanVar(v) => VarLocallyFree.connectiveUsed(v)
      }

    def locallyFree(c: Channel) =
      c.channelInstance match {
        case Quote(p)   => p.locallyFree
        case ChanVar(v) => VarLocallyFree.locallyFree(v)
      }
  }

  implicit val NewLocallyFree: HasLocallyFree[New] = new HasLocallyFree[New] {
    def connectiveUsed(n: New) = n.p.get.connectiveUsed
    def locallyFree(n: New)    = n.locallyFree
  }

  implicit val VarInstanceLocallyFree: HasLocallyFree[VarInstance] =
    new HasLocallyFree[VarInstance] {
      def connectiveUsed(v: VarInstance) =
        v match {
          case BoundVar(_) => false
          case FreeVar(_)  => true
          case Wildcard(_) => true
        }

      def locallyFree(v: VarInstance) =
        v match {
          case BoundVar(level) => BitSet(level)
          case FreeVar(_)      => BitSet()
          case Wildcard(_)     => BitSet()
        }
    }

  implicit val VarLocallyFree: HasLocallyFree[Var] = new HasLocallyFree[Var] {
    def connectiveUsed(v: Var) = VarInstanceLocallyFree.connectiveUsed(v.varInstance)
    def locallyFree(v: Var)    = VarInstanceLocallyFree.locallyFree(v.varInstance)
  }

  implicit val ReceiveLocallyFree: HasLocallyFree[Receive] =
    new HasLocallyFree[Receive] {
      def connectiveUsed(r: Receive) = r.connectiveUsed
      def locallyFree(r: Receive)    = r.locallyFree
    }

  implicit val ReceiveBindLocallyFree: HasLocallyFree[ReceiveBind] =
    new HasLocallyFree[ReceiveBind] {
      def connectiveUsed(rb: ReceiveBind) =
        ChannelLocallyFree.connectiveUsed(rb.source.get)
      def locallyFree(rb: ReceiveBind) =
        ChannelLocallyFree.locallyFree(rb.source.get)
    }

  implicit val MatchLocallyFree: HasLocallyFree[Match] =
    new HasLocallyFree[Match] {
      def connectiveUsed(m: Match) = m.connectiveUsed
      def locallyFree(m: Match)    = m.locallyFree
    }

  implicit val MatchCaseLocallyFree: HasLocallyFree[MatchCase] =
    new HasLocallyFree[MatchCase] {
      def connectiveUsed(mc: MatchCase) = mc.source.get.connectiveUsed
      def locallyFree(mc: MatchCase)    = mc.source.get.locallyFree
    }
}
