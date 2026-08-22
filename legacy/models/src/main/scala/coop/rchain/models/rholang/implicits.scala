package coop.rchain.models.rholang

import com.google.protobuf.ByteString
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.GUnforgeable.UnfInstance
import coop.rchain.models.GUnforgeable.UnfInstance.{
  GDeployIdBody,
  GDeployerIdBody,
  GPrivateBody,
  GSysAuthTokenBody
}
import coop.rchain.models.Var.VarInstance
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models._

import scala.collection.immutable.{BitSet, Vector}

object implicits {

  // Var Related
  def apply(v: VarInstance): Var                                       = new Var(v)
  implicit def fromVarInstance(v: VarInstance): Var                    = apply(v)
  implicit def fromVar[T](v: T)(implicit toVar: T => Var): Option[Var] = Some(v)

  // Expr Related
  def apply(e: ExprInstance)                     = new Expr(exprInstance = e)
  implicit def fromExprInstance(e: ExprInstance) = apply(e)

  implicit def fromGBool(g: GBool): Expr          = apply(g)
  implicit def fromGInt(g: GInt): Expr            = apply(g)
  implicit def fromGBigInt(g: GBigInt): Expr      = apply(g)
  implicit def fromGString(g: GString): Expr      = apply(g)
  implicit def fromGUri(g: GUri): Expr            = apply(g)
  implicit def fromByteArray(g: GByteArray): Expr = apply(g)

  def apply(e: EList): Expr =
    new Expr(exprInstance = EListBody(e))
  implicit def fromEList(e: EList): Expr = apply(e)

  def apply(e: ETuple): Expr =
    new Expr(exprInstance = ETupleBody(e))
  implicit def fromEList(e: ETuple): Expr = apply(e)

  def apply(e: ParSet): Expr =
    new Expr(exprInstance = ESetBody(e))
  implicit def fromESet(e: ParSet): Expr = apply(e)

  def apply(e: ParMap): Expr =
    new Expr(exprInstance = EMapBody(e))
  implicit def fromEMap(e: ParMap): Expr = apply(e)

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

  def apply(e: EMod): Expr =
    new Expr(exprInstance = EModBody(e))
  implicit def fromEMod(e: EMod): Expr = apply(e)

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

  def apply(e: EShortAnd): Expr =
    new Expr(exprInstance = EShortAndBody(e))
  implicit def fromEShortAnd(e: EShortAnd): Expr = apply(e)

  def apply(e: EShortOr): Expr =
    new Expr(exprInstance = EShortOrBody(e))
  implicit def fromEShortOr(e: EShortOr): Expr = apply(e)

  def apply(e: EMethod): Expr =
    new Expr(exprInstance = EMethodBody(e))
  implicit def fromEMethod(e: EMethod): Expr = apply(e)

  def apply(e: EMatches): Expr =
    new Expr(exprInstance = EMatchesBody(e))
  implicit def fromEMatches(e: EMatches): Expr = apply(e)

  def apply(e: EPercentPercent): Expr =
    new Expr(exprInstance = EPercentPercentBody(e))
  implicit def fromEPercent(e: EPercentPercent): Expr = apply(e)

  def apply(e: EPlusPlus): Expr =
    new Expr(exprInstance = EPlusPlusBody(e))
  implicit def fromEPlusPlus(e: EPlusPlus): Expr = apply(e)

  def apply(e: EMinusMinus): Expr =
    new Expr(exprInstance = EMinusMinusBody(e))
  implicit def fromEMinusMinus(e: EMinusMinus): Expr = apply(e)

  // GUnforgeable Related
  def apply(u: UnfInstance)                                  = new GUnforgeable(unfInstance = u)
  implicit def fromUnfInstance(e: UnfInstance): GUnforgeable = apply(e)

  def apply(g: GPrivate): GUnforgeable                 = new GUnforgeable(unfInstance = GPrivateBody(g))
  implicit def fromGPrivate(g: GPrivate): GUnforgeable = apply(g)

  def apply(g: GDeployId): GUnforgeable                  = new GUnforgeable(unfInstance = GDeployIdBody(g))
  implicit def fromGDeployId(g: GDeployId): GUnforgeable = apply(g)

  def apply(g: GDeployerId): GUnforgeable                      = new GUnforgeable(unfInstance = GDeployerIdBody(g))
  implicit def fromGDeployerAuth(g: GDeployerId): GUnforgeable = apply(g)

  def apply(g: GSysAuthToken): GUnforgeable                      = new GUnforgeable(unfInstance = GSysAuthTokenBody(g))
  implicit def fromGSysAuthToken(g: GSysAuthToken): GUnforgeable = apply(g)

  // Par Related
  def apply(): Par = new Par()
  def apply(s: Send): Par =
    new Par(sends = Vector(s), locallyFree = s.locallyFree, connectiveUsed = s.connectiveUsed)
  def apply(r: Receive): Par =
    new Par(receives = Vector(r), locallyFree = r.locallyFree, connectiveUsed = r.connectiveUsed)
  def apply(n: New): Par =
    new Par(
      news = Vector(n),
      locallyFree = NewLocallyFree.locallyFree(n, 0),
      connectiveUsed = NewLocallyFree.connectiveUsed(n)
    )
  def apply(e: Expr): Par =
    new Par(
      exprs = Vector(e),
      locallyFree = ExprLocallyFree.locallyFree(e, 0),
      connectiveUsed = ExprLocallyFree.connectiveUsed(e)
    )
  def apply(m: Match): Par =
    new Par(matches = Vector(m), locallyFree = m.locallyFree, connectiveUsed = m.connectiveUsed)
  def apply(g: GUnforgeable): Par =
    new Par(
      unforgeables = Vector(g),
      locallyFree = BitSet(),
      connectiveUsed = false
    )
  def apply(b: Bundle): Par =
    new Par(
      bundles = Vector(b),
      locallyFree = b.body.locallyFree,
      connectiveUsed = false
    )

  def apply(c: Connective): Par =
    new Par(connectives = Vector(c), connectiveUsed = ConnectiveLocallyFree.connectiveUsed(c))

  implicit def fromSend(s: Send): Par                                                     = apply(s)
  implicit def fromReceive(r: Receive): Par                                               = apply(r)
  implicit def fromNew(n: New): Par                                                       = apply(n)
  implicit def fromExpr[T](e: T)(implicit toExpr: T => Expr): Par                         = apply(e)
  implicit def fromMatch(m: Match): Par                                                   = apply(m)
  implicit def fromGUnforgeable[T](g: T)(implicit toGUnforgeable: T => GUnforgeable): Par = apply(g)
  implicit def fromBundle(b: Bundle): Par                                                 = apply(b)
  implicit def fromConnective(c: Connective): Par                                         = apply(c)

  object VectorPar {
    def apply(): Par = new Par(
      sends = Vector.empty[Send],
      receives = Vector.empty[Receive],
      news = Vector.empty[New],
      exprs = Vector.empty[Expr],
      matches = Vector.empty[Match],
      unforgeables = Vector.empty[GUnforgeable],
      bundles = Vector.empty[Bundle],
      connectives = Vector.empty[Connective]
    )
  }

  object GPrivateBuilder {
    def apply(): GUnforgeable =
      GUnforgeable(
        GPrivateBody(new GPrivate(ByteString.copyFromUtf8(java.util.UUID.randomUUID.toString)))
      )
    def apply(s: String): GUnforgeable =
      GUnforgeable(GPrivateBody(new GPrivate(ByteString.copyFromUtf8(s))))
    def apply(b: ByteString): GUnforgeable = GUnforgeable(GPrivateBody(new GPrivate(b)))
  }

  implicit class RichExprInstance(exprInstance: ExprInstance) {
    def typ: String =
      exprInstance match {
        case GBool(_)      => "Bool"
        case GInt(_)       => "Int"
        case GBigInt(_)    => "BigInt"
        case GString(_)    => "String"
        case GUri(_)       => "Uri"
        case GByteArray(_) => "ByteArray"
        case EListBody(_)  => "List"
        case ETupleBody(_) => "Tuple"
        case ESetBody(_)   => "Set"
        case EMapBody(_)   => "Map"
        case _             => "Unit"
      }
  }

  implicit class ParExtension[T](p: T)(implicit toPar: T => Par) {
    // Convenience prepend methods
    def prepend(s: Send): Par =
      p.copy(
        sends = s +: p.sends,
        locallyFree = p.locallyFree | s.locallyFree,
        connectiveUsed = p.connectiveUsed || s.connectiveUsed
      )
    def prepend(r: Receive): Par =
      p.copy(
        receives = r +: p.receives,
        locallyFree = p.locallyFree | r.locallyFree,
        connectiveUsed = p.connectiveUsed || r.connectiveUsed
      )
    def prepend(n: New): Par =
      p.copy(
        news = n +: p.news,
        locallyFree = p.locallyFree | n.locallyFree,
        connectiveUsed = p.connectiveUsed || n.connectiveUsed
      )
    def prepend(e: Expr, depth: Int): Par =
      p.copy(
        exprs = e +: p.exprs,
        locallyFree = p.locallyFree | ExprLocallyFree.locallyFree(e, depth),
        connectiveUsed = p.connectiveUsed || ExprLocallyFree.connectiveUsed(e)
      )
    def prepend(m: Match): Par =
      p.copy(
        matches = m +: p.matches,
        locallyFree = p.locallyFree | m.locallyFree,
        connectiveUsed = p.connectiveUsed || m.connectiveUsed
      )
    def prepend(b: Bundle): Par =
      p.copy(
        bundles = b +: p.bundles,
        locallyFree = b.body.locallyFree | p.locallyFree
      )
    def prepend(c: Connective, depth: Int): Par =
      p.copy(
        connectives = c +: p.connectives,
        connectiveUsed = p.connectiveUsed || ConnectiveLocallyFree.connectiveUsed(c),
        locallyFree = ConnectiveLocallyFree.locallyFree(c, depth)
      )

    def singleExpr(): Option[Expr] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.matches.isEmpty && p.bundles.isEmpty) {
        p.exprs match {
          case Seq(single) => Some(single)
          case _           => None
        }
      } else {
        None
      }

    def isNil(): Boolean =
      p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.matches.isEmpty && p.bundles.isEmpty && p.exprs.isEmpty && p.unforgeables.isEmpty && p.connectives.isEmpty

    def singleBundle(): Option[Bundle] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.exprs.isEmpty && p.matches.isEmpty && p.unforgeables.isEmpty && p.connectives.isEmpty) {
        p.bundles match {
          case Seq(single) => Some(single)
          case _           => None
        }
      } else {
        None
      }

    def singleUnforgeable(): Option[GUnforgeable] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.exprs.isEmpty && p.matches.isEmpty && p.bundles.isEmpty && p.connectives.isEmpty) {
        p.unforgeables match {
          case Seq(single) => Some(single)
          case _           => None
        }
      } else {
        None
      }

    def singleDeployId(): Option[GDeployId] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.exprs.isEmpty && p.matches.isEmpty && p.bundles.isEmpty && p.connectives.isEmpty) {
        p.unforgeables match {
          case Seq(GUnforgeable(GDeployIdBody(single))) => Some(single)
          case _                                        => None
        }
      } else {
        None
      }

    def singleDeployerId(): Option[GDeployerId] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.exprs.isEmpty && p.matches.isEmpty && p.bundles.isEmpty && p.connectives.isEmpty) {
        p.unforgeables match {
          case Seq(GUnforgeable(GDeployerIdBody(single))) => Some(single)
          case _                                          => None
        }
      } else {
        None
      }

    def singleConnective(): Option[Connective] =
      if (p.sends.isEmpty && p.receives.isEmpty && p.news.isEmpty && p.exprs.isEmpty && p.matches.isEmpty && p.bundles.isEmpty && p.connectives.size == 1) {
        Some(p.connectives.head)
      } else {
        None
      }

    def ++(that: Par): Par =
      Par(
        that.sends ++ p.sends,
        that.receives ++ p.receives,
        that.news ++ p.news,
        that.exprs ++ p.exprs,
        that.matches ++ p.matches,
        that.unforgeables ++ p.unforgeables,
        that.bundles ++ p.bundles,
        that.connectives ++ p.connectives,
        that.locallyFree | p.locallyFree,
        that.connectiveUsed || p.connectiveUsed
      )
  }

  implicit val ParLocallyFree: HasLocallyFree[Par] = new HasLocallyFree[Par] {
    def connectiveUsed(p: Par)          = p.connectiveUsed
    def locallyFree(p: Par, depth: Int) = p.locallyFree
  }

  implicit val BundleLocallyFree: HasLocallyFree[Bundle] = new HasLocallyFree[Bundle] {
    override def connectiveUsed(source: Bundle): Boolean         = false
    override def locallyFree(source: Bundle, depth: Int): BitSet = source.body.locallyFree
  }

  implicit val SendLocallyFree: HasLocallyFree[Send] = new HasLocallyFree[Send] {
    def connectiveUsed(s: Send)          = s.connectiveUsed
    def locallyFree(s: Send, depth: Int) = s.locallyFree
  }

  implicit val UnforgeableLocallyFree: HasLocallyFree[GUnforgeable] =
    new HasLocallyFree[GUnforgeable] {
      def connectiveUsed(unf: GUnforgeable)                     = false
      def locallyFree(source: GUnforgeable, depth: Int): BitSet = BitSet()
    }

  implicit val ExprLocallyFree: HasLocallyFree[Expr] = new HasLocallyFree[Expr] {
    def connectiveUsed(e: Expr) =
      e.exprInstance match {
        case GBool(_)                                     => false
        case GInt(_)                                      => false
        case GBigInt(_)                                   => false
        case GString(_)                                   => false
        case GUri(_)                                      => false
        case GByteArray(_)                                => false
        case EListBody(e)                                 => e.connectiveUsed
        case ETupleBody(e)                                => e.connectiveUsed
        case ESetBody(e)                                  => e.connectiveUsed
        case EMapBody(e)                                  => e.connectiveUsed
        case EVarBody(EVar(v))                            => VarLocallyFree.connectiveUsed(v)
        case ENotBody(ENot(p))                            => p.connectiveUsed
        case ENegBody(ENeg(p))                            => p.connectiveUsed
        case EMultBody(EMult(p1, p2))                     => p1.connectiveUsed || p2.connectiveUsed
        case EDivBody(EDiv(p1, p2))                       => p1.connectiveUsed || p2.connectiveUsed
        case EModBody(EMod(p1, p2))                       => p1.connectiveUsed || p2.connectiveUsed
        case EPlusBody(EPlus(p1, p2))                     => p1.connectiveUsed || p2.connectiveUsed
        case EMinusBody(EMinus(p1, p2))                   => p1.connectiveUsed || p2.connectiveUsed
        case ELtBody(ELt(p1, p2))                         => p1.connectiveUsed || p2.connectiveUsed
        case ELteBody(ELte(p1, p2))                       => p1.connectiveUsed || p2.connectiveUsed
        case EGtBody(EGt(p1, p2))                         => p1.connectiveUsed || p2.connectiveUsed
        case EGteBody(EGte(p1, p2))                       => p1.connectiveUsed || p2.connectiveUsed
        case EEqBody(EEq(p1, p2))                         => p1.connectiveUsed || p2.connectiveUsed
        case ENeqBody(ENeq(p1, p2))                       => p1.connectiveUsed || p2.connectiveUsed
        case EAndBody(EAnd(p1, p2))                       => p1.connectiveUsed || p2.connectiveUsed
        case EOrBody(EOr(p1, p2))                         => p1.connectiveUsed || p2.connectiveUsed
        case EShortAndBody(EShortAnd(p1, p2))             => p1.connectiveUsed || p2.connectiveUsed
        case EShortOrBody(EShortOr(p1, p2))               => p1.connectiveUsed || p2.connectiveUsed
        case EMethodBody(e)                               => e.connectiveUsed
        case EMatchesBody(EMatches(target, pattern @ _))  => target.connectiveUsed
        case EPercentPercentBody(EPercentPercent(p1, p2)) => p1.connectiveUsed || p2.connectiveUsed
        case EPlusPlusBody(EPlusPlus(p1, p2))             => p1.connectiveUsed || p2.connectiveUsed
        case EMinusMinusBody(EMinusMinus(p1, p2))         => p1.connectiveUsed || p2.connectiveUsed
        case ExprInstance.Empty                           => false
      }

    def locallyFree(e: Expr, depth: Int) =
      e.exprInstance match {
        case GBool(_)                                     => BitSet()
        case GInt(_)                                      => BitSet()
        case GBigInt(_)                                   => BitSet()
        case GString(_)                                   => BitSet()
        case GUri(_)                                      => BitSet()
        case GByteArray(_)                                => BitSet()
        case EListBody(e)                                 => e.locallyFree
        case ETupleBody(e)                                => e.locallyFree
        case ESetBody(e)                                  => e.locallyFree.value
        case EMapBody(e)                                  => e.locallyFree.value
        case EVarBody(EVar(v))                            => VarLocallyFree.locallyFree(v, depth)
        case ENotBody(ENot(p))                            => p.locallyFree
        case ENegBody(ENeg(p))                            => p.locallyFree
        case EMultBody(EMult(p1, p2))                     => p1.locallyFree | p2.locallyFree
        case EDivBody(EDiv(p1, p2))                       => p1.locallyFree | p2.locallyFree
        case EModBody(EMod(p1, p2))                       => p1.locallyFree | p2.locallyFree
        case EPlusBody(EPlus(p1, p2))                     => p1.locallyFree | p2.locallyFree
        case EMinusBody(EMinus(p1, p2))                   => p1.locallyFree | p2.locallyFree
        case ELtBody(ELt(p1, p2))                         => p1.locallyFree | p2.locallyFree
        case ELteBody(ELte(p1, p2))                       => p1.locallyFree | p2.locallyFree
        case EGtBody(EGt(p1, p2))                         => p1.locallyFree | p2.locallyFree
        case EGteBody(EGte(p1, p2))                       => p1.locallyFree | p2.locallyFree
        case EEqBody(EEq(p1, p2))                         => p1.locallyFree | p2.locallyFree
        case ENeqBody(ENeq(p1, p2))                       => p1.locallyFree | p2.locallyFree
        case EAndBody(EAnd(p1, p2))                       => p1.locallyFree | p2.locallyFree
        case EOrBody(EOr(p1, p2))                         => p1.locallyFree | p2.locallyFree
        case EShortAndBody(EShortAnd(p1, p2))             => p1.locallyFree | p2.locallyFree
        case EShortOrBody(EShortOr(p1, p2))               => p1.locallyFree | p2.locallyFree
        case EMethodBody(e)                               => e.locallyFree
        case EMatchesBody(EMatches(target, pattern))      => target.locallyFree | pattern.locallyFree
        case EPercentPercentBody(EPercentPercent(p1, p2)) => p1.locallyFree | p2.locallyFree
        case EPlusPlusBody(EPlusPlus(p1, p2))             => p1.locallyFree | p2.locallyFree
        case EMinusMinusBody(EMinusMinus(p1, p2))         => p1.locallyFree | p2.locallyFree
        case ExprInstance.Empty                           => BitSet()
      }
  }

  implicit val NewLocallyFree: HasLocallyFree[New] = new HasLocallyFree[New] {
    def connectiveUsed(n: New)          = n.p.connectiveUsed
    def locallyFree(n: New, depth: Int) = n.locallyFree
  }

  implicit val VarInstanceLocallyFree: HasLocallyFree[VarInstance] =
    new HasLocallyFree[VarInstance] {
      def connectiveUsed(v: VarInstance) =
        v match {
          case BoundVar(_)       => false
          case FreeVar(_)        => true
          case Wildcard(_)       => true
          case VarInstance.Empty => false
        }

      def locallyFree(v: VarInstance, depth: Int) =
        v match {
          case BoundVar(index)   => if (depth == 0) BitSet(index) else BitSet()
          case FreeVar(_)        => BitSet()
          case Wildcard(_)       => BitSet()
          case VarInstance.Empty => BitSet()
        }
    }

  implicit val VarLocallyFree: HasLocallyFree[Var] = new HasLocallyFree[Var] {
    def connectiveUsed(v: Var)          = VarInstanceLocallyFree.connectiveUsed(v.varInstance)
    def locallyFree(v: Var, depth: Int) = VarInstanceLocallyFree.locallyFree(v.varInstance, depth)
  }

  implicit val ReceiveLocallyFree: HasLocallyFree[Receive] =
    new HasLocallyFree[Receive] {
      def connectiveUsed(r: Receive)          = r.connectiveUsed
      def locallyFree(r: Receive, depth: Int) = r.locallyFree
    }

  implicit val ReceiveBindLocallyFree: HasLocallyFree[ReceiveBind] =
    new HasLocallyFree[ReceiveBind] {
      def connectiveUsed(rb: ReceiveBind) =
        ParLocallyFree.connectiveUsed(rb.source)

      def locallyFree(rb: ReceiveBind, depth: Int) =
        ParLocallyFree.locallyFree(rb.source, depth) |
          rb.patterns.foldLeft(BitSet()) { (acc, pat) =>
            acc | ParLocallyFree.locallyFree(pat, depth + 1)
          }
    }

  implicit val MatchLocallyFree: HasLocallyFree[Match] =
    new HasLocallyFree[Match] {
      def connectiveUsed(m: Match)          = m.connectiveUsed
      def locallyFree(m: Match, depth: Int) = m.locallyFree
    }

  implicit val MatchCaseLocallyFree: HasLocallyFree[MatchCase] =
    new HasLocallyFree[MatchCase] {
      def connectiveUsed(mc: MatchCase) = mc.source.connectiveUsed
      def locallyFree(mc: MatchCase, depth: Int) =
        mc.source.locallyFree | ParLocallyFree.locallyFree(mc.pattern, depth + 1)
    }

  implicit val ConnectiveLocallyFree: HasLocallyFree[Connective] =
    new HasLocallyFree[Connective] {
      def connectiveUsed(conn: Connective) =
        conn.connectiveInstance match {
          case ConnAndBody(_)           => true
          case ConnOrBody(_)            => true
          case ConnNotBody(_)           => true
          case VarRefBody(_)            => false
          case _: ConnBool              => true
          case _: ConnInt               => true
          case _: ConnBigInt            => true
          case _: ConnString            => true
          case _: ConnUri               => true
          case _: ConnByteArray         => true
          case ConnectiveInstance.Empty => false
        }
      def locallyFree(conn: Connective, depth: Int) =
        conn.connectiveInstance match {
          case VarRefBody(VarRef(idx, varDepth)) =>
            if (depth == varDepth)
              BitSet(idx)
            else
              BitSet()
          case _ => BitSet()
        }
    }
}
