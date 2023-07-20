package coop.rchain.models.rholangN

import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.GUnforgeable.UnfInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import scalapb.GeneratedMessage

import scala.annotation.unused

private[rholangN] object BindingsFromProto {

  def fromProto(p: Par): ParN = {
    val terms: Seq[GeneratedMessage] =
      Seq(p.sends, p.receives, p.news, p.exprs, p.matches, p.unforgeables, p.bundles, p.connectives)
        .filter(_.nonEmpty)
        .flatten
    val ps: Seq[ParN] = terms.map(fromProtoMessage)
    ps.size match {
      case 0 => NilN()
      case 1 => ps.head
      case _ => ParProcN(ps)
    }
  }

  private def fromProtoMessage(m: GeneratedMessage): ParN = m match {

    /** Basic types */
    case x: Send    => fromSend(x)
    case x: Receive => fromReceive(x)
    case x: Match   => fromMatch(x)
    case x: New     => fromNew(x)

    case e: Expr =>
      e.exprInstance match {

        /** Ground types */
        case x: GBool      => fromGBool(x)
        case x: GInt       => fromGInt(x)
        case x: GBigInt    => fromGBigInt(x)
        case x: GString    => fromGString(x)
        case x: GByteArray => fromGByteArray(x)
        case x: GUri       => fromGUri(x)

        /** Collections */
        case x: EListBody  => fromEList(x.value)
        case x: ETupleBody => fromETuple(x.value)
        case x: ESetBody   => fromParSet(x.value)
        case x: EMapBody   => fromParMap(x.value)

        /** Vars */
        case x: EVarBody =>
          x.value.v.varInstance match {
            case n: BoundVar => fromBoundVar(n)
            case n: FreeVar  => fromFreeVar(n)
            case n: Wildcard => fromWildcard(n)
            case _ =>
              assert(assertion = false, "Unknown type for Var conversion")
              WildcardN()
          }

        /** Operations */
        case x: ENegBody            => fromENeg(x.value)
        case x: ENotBody            => fromENot(x.value)
        case x: EPlusBody           => fromEPlus(x.value)
        case x: EMinusBody          => fromEMinus(x.value)
        case x: EMultBody           => fromEMult(x.value)
        case x: EDivBody            => fromEDiv(x.value)
        case x: EModBody            => fromEMod(x.value)
        case x: ELtBody             => fromELt(x.value)
        case x: ELteBody            => fromELte(x.value)
        case x: EGtBody             => fromEGt(x.value)
        case x: EGteBody            => fromEGte(x.value)
        case x: EEqBody             => fromEEq(x.value)
        case x: ENeqBody            => fromENeq(x.value)
        case x: EAndBody            => fromEAnd(x.value)
        case x: EShortAndBody       => fromEShortAnd(x.value)
        case x: EOrBody             => fromEOr(x.value)
        case x: EShortOrBody        => fromEShortOr(x.value)
        case x: EPlusPlusBody       => fromEPlusPlus(x.value)
        case x: EMinusMinusBody     => fromEMinusMinus(x.value)
        case x: EPercentPercentBody => fromEPercentPercent(x.value)
        case x: EMethodBody         => fromEMethod(x.value)
        case x: EMatchesBody        => fromEMatches(x.value)

        case _ =>
          assert(assertion = false, "Unknown type for Expr conversion")
          GBoolN(true)
      }

    /** Unforgeable names */
    case u: GUnforgeable =>
      u.unfInstance match {
        case x: GPrivateBody      => fromPrivate(x.value)
        case x: GDeployIdBody     => fromDeployId(x.value)
        case x: GDeployerIdBody   => fromDeployerId(x.value)
        case x: GSysAuthTokenBody => fromGSysAuthToken(x.value)
        case _ =>
          assert(assertion = false, "Unknown type for GUnforgeable conversion")
          UPrivateN(Array(0x04.toByte, 0x02.toByte))
      }

    /** Connective */
    case c: Connective =>
      c.connectiveInstance match {
        case x: ConnBool      => fromConnBool(x)
        case x: ConnInt       => fromConnInt(x)
        case x: ConnBigInt    => fromConnBigInt(x)
        case x: ConnString    => fromConnString(x)
        case x: ConnUri       => fromConnUri(x)
        case x: ConnByteArray => fromConnByteArray(x)
        case x: ConnNotBody   => fromConnNotBody(x)
        case x: ConnAndBody   => fromConnAndBody(x)
        case x: ConnOrBody    => fromConnOrBody(x)
        case x: VarRefBody    => fromVarRefBody(x)
        case _ =>
          assert(assertion = false, "Unknown type for Connective conversion")
          ConnBoolN()
      }

    /** Other types */
    case x: Bundle => fromBundle(x)
  }

  private def fromProto(ps: Seq[Par]): Seq[ParN]           = ps.map(fromProto)
  private def fromProto(varOpt: Option[Var]): Option[VarN] = varOpt.map(fromVar)
  private def fromProtoKVPairs(ps: Seq[(Par, Par)]): Seq[(ParN, ParN)] =
    ps.map(kv => (fromProto(kv._1), fromProto(kv._2)))
  private def fromProtoInjections(ps: Seq[(String, Par)]): Seq[(String, ParN)] =
    ps.map(kv => (kv._1, fromProto(kv._2)))

  /** Basic types */
  private def fromSend(x: Send): SendN = {
    val chan       = fromProto(x.chan)
    val data       = fromProto(x.data)
    val persistent = x.persistent
    SendN(chan, data, persistent)
  }

  private def fromReceive(x: Receive): ReceiveN = {
    val binds      = x.binds.map(fromReceiveBind)
    val body       = fromProto(x.body)
    val persistent = x.persistent
    val peek       = x.peek
    val bindCount  = x.bindCount
    ReceiveN(binds, body, persistent, peek, bindCount)
  }

  private def fromReceiveBind(x: ReceiveBind): ReceiveBindN = {
    val patterns  = fromProto(x.patterns)
    val source    = fromProto(x.source)
    val remainder = fromProto(x.remainder)
    val freeCount = x.freeCount
    ReceiveBindN(patterns, source, remainder, freeCount)
  }

  private def fromMatch(x: Match): MatchN = {
    val target = fromProto(x.target)
    val cases  = x.cases.map(fromMatchCase)
    MatchN(target, cases)
  }

  private def fromMatchCase(x: MatchCase): MatchCaseN = {
    val pattern   = fromProto(x.pattern)
    val source    = fromProto(x.source)
    val freeCount = x.freeCount
    MatchCaseN(pattern, source, freeCount)
  }

  private def fromNew(x: New): NewN = {
    val bindCount                       = x.bindCount
    val p                               = fromProto(x.p)
    val uri                             = x.uri
    val injections: Seq[(String, ParN)] = fromProtoInjections(x.injections.toSeq)
    NewN(bindCount, p, uri, injections)
  }

  /** Ground types */
  private def fromGBool(x: GBool): GBoolN = {
    val v = x.value
    GBoolN(v)
  }

  private def fromGInt(x: GInt): GIntN = {
    val v = x.value
    GIntN(v)
  }

  private def fromGBigInt(x: GBigInt): GBigIntN = {
    val v = x.value
    GBigIntN(v)
  }

  private def fromGString(x: GString): GStringN = {
    val v = x.value
    GStringN(v)
  }

  private def fromGByteArray(x: GByteArray): GByteArrayN = {
    val v = x.value.toByteArray
    GByteArrayN(v)
  }

  private def fromGUri(x: GUri): GUriN = {
    val v = x.value
    GUriN(v)
  }

  /** Collections */
  private def fromEList(x: EList): EListN = {
    val ps        = fromProto(x.ps)
    val remainder = fromProto(x.remainder)
    EListN(ps, remainder)
  }

  private def fromETuple(x: ETuple): ETupleN = {
    val ps = fromProto(x.ps)
    ETupleN(ps)
  }

  private def fromParSet(x: ParSet): ESetN = {
    val ps        = fromProto(x.ps.sortedPars)
    val remainder = fromProto(x.remainder)
    ESetN(ps, remainder)
  }

  private def fromParMap(x: ParMap): EMapN = {
    val ps        = fromProtoKVPairs(x.ps.sortedList)
    val remainder = fromProto(x.remainder)
    EMapN(ps, remainder)
  }

  /** Vars */
  private def fromBoundVar(x: BoundVar): BoundVarN = {
    val idx = x.value
    BoundVarN(idx)
  }

  private def fromFreeVar(x: FreeVar): FreeVarN = {
    val idx = x.value
    FreeVarN(idx)
  }

  private def fromWildcard(@unused x: Wildcard): WildcardN =
    WildcardN()

  def fromVar(x: Var): VarN = x.varInstance match {
    case n: BoundVar => fromBoundVar(n)
    case n: FreeVar  => fromFreeVar(n)
    case n: Wildcard => fromWildcard(n)
    case _ =>
      assert(assertion = false, "Unknown type for Var conversion")
      WildcardN()
  }

  /** Unforgeable names */
  private def fromPrivate(x: GPrivate): UPrivateN = {
    val v = x.id.toByteArray
    UPrivateN(v)
  }

  private def fromDeployId(x: GDeployId): UDeployIdN = {
    val v = x.sig.toByteArray
    UDeployIdN(v)
  }

  private def fromDeployerId(x: GDeployerId): UDeployerIdN = {
    val v = x.publicKey.toByteArray
    UDeployerIdN(v)
  }

  /** Operations */
  private def fromENeg(x: ENeg): ENegN = {
    val p = fromProto(x.p)
    ENegN(p)
  }

  private def fromENot(x: ENot): ENotN = {
    val p = fromProto(x.p)
    ENotN(p)
  }

  private def fromEPlus(x: EPlus): EPlusN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EPlusN(p1, p2)
  }

  private def fromEMinus(x: EMinus): EMinusN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EMinusN(p1, p2)
  }

  private def fromEMult(x: EMult): EMultN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EMultN(p1, p2)
  }

  private def fromEDiv(x: EDiv): EDivN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EDivN(p1, p2)
  }

  private def fromEMod(x: EMod): EModN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EModN(p1, p2)
  }

  private def fromELt(x: ELt): ELtN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    ELtN(p1, p2)
  }

  private def fromELte(x: ELte): ELteN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    ELteN(p1, p2)
  }

  private def fromEGt(x: EGt): EGtN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EGtN(p1, p2)
  }

  private def fromEGte(x: EGte): EGteN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EGteN(p1, p2)
  }

  private def fromEEq(x: EEq): EEqN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EEqN(p1, p2)
  }

  private def fromENeq(x: ENeq): ENeqN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    ENeqN(p1, p2)
  }

  private def fromEAnd(x: EAnd): EAndN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EAndN(p1, p2)
  }

  private def fromEShortAnd(x: EShortAnd): EShortAndN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EShortAndN(p1, p2)
  }

  private def fromEOr(x: EOr): EOrN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EOrN(p1, p2)
  }

  private def fromEShortOr(x: EShortOr): EShortOrN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EShortOrN(p1, p2)
  }

  private def fromEPlusPlus(x: EPlusPlus): EPlusPlusN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EPlusPlusN(p1, p2)
  }

  private def fromEMinusMinus(x: EMinusMinus): EMinusMinusN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EMinusMinusN(p1, p2)
  }

  private def fromEPercentPercent(x: EPercentPercent): EPercentPercentN = {
    val p1 = fromProto(x.p1)
    val p2 = fromProto(x.p2)
    EPercentPercentN(p1, p2)
  }

  private def fromEMethod(x: EMethod): EMethodN = {
    val methodName = x.methodName
    val target     = fromProto(x.target)
    val arguments  = fromProto(x.arguments)
    EMethodN(methodName, target, arguments)
  }

  private def fromEMatches(x: EMatches): EMatchesN = {
    val target  = fromProto(x.target)
    val pattern = fromProto(x.pattern)
    EMatchesN(target, pattern)
  }

  /** Connective */
  private def fromConnBool(@unused x: ConnBool): ConnBoolN =
    ConnBoolN()

  private def fromConnInt(@unused x: ConnInt): ConnIntN =
    ConnIntN()

  private def fromConnBigInt(@unused x: ConnBigInt): ConnBigIntN =
    ConnBigIntN()

  private def fromConnString(@unused x: ConnString): ConnStringN =
    ConnStringN()

  private def fromConnUri(@unused x: ConnUri): ConnUriN =
    ConnUriN()

  private def fromConnByteArray(@unused x: ConnByteArray): ConnByteArrayN =
    ConnByteArrayN()

  private def fromConnNotBody(x: ConnNotBody): ConnNotN = {
    val p = fromProto(x.value)
    ConnNotN(p)
  }

  private def fromConnAndBody(x: ConnAndBody): ConnAndN = {
    val ps = fromProto(x.value.ps)
    ConnAndN(ps)
  }

  private def fromConnOrBody(x: ConnOrBody): ConnOrN = {
    val ps = fromProto(x.value.ps)
    ConnOrN(ps)
  }

  private def fromVarRefBody(x: VarRefBody): ConnVarRefN = {
    val index = x.value.index
    val depth = x.value.depth
    ConnVarRefN(index, depth)
  }

  /** Other types */
  private def fromBundle(x: Bundle): BundleN = {
    val body      = fromProto(x.body)
    val writeFlag = x.writeFlag
    val readFlag  = x.readFlag
    BundleN(body, writeFlag, readFlag)
  }

  private def fromGSysAuthToken(@unused x: GSysAuthToken): SysAuthTokenN =
    SysAuthTokenN()
}
