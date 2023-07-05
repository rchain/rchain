package coop.rchain.models.rholangN

import cats.Eval
import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.effect.implicits.sEval
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.Var.WildcardMsg
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._

import scala.collection.immutable.BitSet

private[rholangN] object BindingsToProto {
  def toProto(p: ParN): Par = p match {

    /** Basic types */
    case x: NilN     => toNil(x)
    case x: ParProcN => toParProc(x)
    case x: SendN    => toSend(x)
    case x: ReceiveN => toReceive(x)
    case x: MatchN   => toMatch(x)
    case x: NewN     => toNew(x)

    /** Ground types */
    case x: GBoolN      => toGBool(x)
    case x: GIntN       => toGInt(x)
    case x: GBigIntN    => toGBigInt(x)
    case x: GStringN    => toGString(x)
    case x: GByteArrayN => toGByteArray(x)
    case x: GUriN       => toGUri(x)

    /** Collections */
    case x: EListN  => toEList(x)
    case x: ETupleN => toETuple(x)
    case x: ESetN   => toParSet(x)
    case x: EMapN   => toParMap(x)

    /** Vars */
    case x: BoundVarN => EVar(toBoundVar(x))
    case x: FreeVarN  => EVar(toFreeVar(x))
    case x: WildcardN => EVar(toWildcard(x))

    /** Unforgeable names */
    case x: UPrivateN    => toPrivate(x)
    case x: UDeployIdN   => toDeployId(x)
    case x: UDeployerIdN => toDeployerId(x)

    /** Operations */
    case x: ENegN            => toENeg(x)
    case x: ENotN            => toENot(x)
    case x: EPlusN           => toEPlus(x)
    case x: EMinusN          => toEMinus(x)
    case x: EMultN           => toEMult(x)
    case x: EDivN            => toEDiv(x)
    case x: EModN            => toEMod(x)
    case x: ELtN             => toELt(x)
    case x: ELteN            => toELte(x)
    case x: EGtN             => toEGt(x)
    case x: EGteN            => toEGte(x)
    case x: EEqN             => toEEq(x)
    case x: ENeqN            => toENeq(x)
    case x: EAndN            => toEAnd(x)
    case x: EShortAndN       => toEShortAnd(x)
    case x: EOrN             => toEOr(x)
    case x: EShortOrN        => toEShortOr(x)
    case x: EPlusPlusN       => toEPlusPlus(x)
    case x: EMinusMinusN     => toEMinusMinus(x)
    case x: EPercentPercentN => toEPercentPercent(x)
    case x: EMethodN         => toEMethod(x)
    case x: EMatchesN        => toEMatches(x)

    /** Connective */
    case x: ConnBoolN      => Connective(toConnBool(x))
    case x: ConnIntN       => Connective(toConnInt(x))
    case x: ConnBigIntN    => Connective(toConnBigInt(x))
    case x: ConnStringN    => Connective(toConnString(x))
    case x: ConnUriN       => Connective(toConnUri(x))
    case x: ConnByteArrayN => Connective(toConnByteArray(x))
    case x: ConnNotN       => Connective(toConnNotBody(x))
    case x: ConnAndN       => Connective(toConnAndBody(x))
    case x: ConnOrN        => Connective(toConnOrBody(x))
    case x: ConnVarRefN    => Connective(toVarRefBody(x))

    /** Other types */
    case x: BundleN       => toBundle(x)
    case x: SysAuthTokenN => toGSysAuthToken(x)

    /** Unknown types */
    case _ =>
      assert(assertion = false, "Invalid Var type")
      Par()
  }

  def toProto(ps: Seq[ParN]): Seq[Par]           = ps.map(toProto)
  def toProto(varOpt: Option[VarN]): Option[Var] = varOpt.map(toVar)
  def toProtoKVPairs(ps: Seq[(ParN, ParN)]): Seq[(Par, Par)] =
    ps.map(kv => (toProto(kv._1), toProto(kv._2)))

  /** Basic types */
  def toNil(x: NilN): Par = Par()

  def toParProc(x: ParProcN): Par = {
    val p = x.ps.foldLeft(Par())((acc, pN) => acc ++ toProto(pN))
    p.withConnectiveUsed(x.connectiveUsed)
  }

  def toSend(x: SendN): Send = {
    val chan           = toProto(x.chan)
    val data           = toProto(x.data)
    val persistent     = x.persistent
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed
    Send(chan, data, persistent, locallyFree, connectiveUsed)
  }

  def toReceive(x: ReceiveN): Receive = {
    val binds          = x.binds.map(toReceiveBind)
    val body           = toProto(x.body)
    val persistent     = x.persistent
    val peek           = x.peek
    val bindCount      = x.bindCount
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed
    Receive(binds, body, persistent, peek, bindCount, locallyFree, connectiveUsed)
  }

  def toReceiveBind(x: ReceiveBindN): ReceiveBind = {
    val patterns  = toProto(x.patterns)
    val source    = toProto(x.source)
    val remainder = toProto(x.remainder)
    val freeCount = x.freeCount
    ReceiveBind(patterns, source, remainder, freeCount)
  }

  def toMatch(x: MatchN): Match = {
    val target         = toProto(x.target)
    val cases          = x.cases.map(toMatchCase)
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed
    Match(target, cases, locallyFree, connectiveUsed)
  }

  def toMatchCase(x: MatchCaseN): MatchCase = {
    val pattern   = toProto(x.pattern)
    val source    = toProto(x.source)
    val freeCount = x.freeCount
    MatchCase(pattern, source, freeCount)
  }

  def toNew(x: NewN): New = {
    val bindCount                    = x.bindCount
    val p                            = toProto(x.p)
    val uri                          = x.uri
    val injections: Map[String, Par] = Map()
    val locallyFree                  = BitSet()
    New(bindCount, p, uri, injections, locallyFree)
  }

  /** Ground types */
  def toGBool(x: GBoolN): GBool = {
    val v = x.v
    GBool(v)
  }

  def toGInt(x: GIntN): GInt = {
    val v = x.v
    GInt(v)
  }

  def toGBigInt(x: GBigIntN): GBigInt = {
    val v = x.v
    GBigInt(v)
  }

  def toGString(x: GStringN): GString = {
    val v = x.v
    GString(v)
  }

  def toGByteArray(x: GByteArrayN): GByteArray = {
    val v = ByteString.copyFrom(x.v.toArray)
    GByteArray(v)
  }

  def toGUri(x: GUriN): GUri = {
    val v = x.v
    GUri(v)
  }

  /** Collections */
  def toEList(x: EListN): EList = {
    val ps             = toProto(x.ps)
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed
    val remainder      = toProto(x.remainder)
    EList(ps, locallyFree, connectiveUsed, remainder)
  }

  def toETuple(x: ETupleN): ETuple = {
    val ps             = toProto(x.ps)
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed
    ETuple(ps, locallyFree, connectiveUsed)
  }

  def toParSet(x: ESetN): ParSet = {
    val ps             = toProto(x.sortedPs)
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed
    val remainder      = toProto(x.remainder)
    ParSet(ps, connectiveUsed, Sync[Eval].delay(locallyFree), remainder)
  }

  def toParMap(x: EMapN): ParMap = {
    val ps             = toProtoKVPairs(x.sortedPs)
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed
    val remainder      = toProto(x.remainder)
    ParMap(ps, connectiveUsed, Sync[Eval].delay(locallyFree), remainder)
  }

  /** Vars */
  def toBoundVar(x: BoundVarN): BoundVar = {
    val idx = x.idx
    BoundVar(idx)
  }

  def toFreeVar(x: FreeVarN): FreeVar = {
    val idx = x.idx
    FreeVar(idx)
  }

  def toWildcard(x: WildcardN): Wildcard =
    Wildcard(WildcardMsg())

  def toVar(x: VarN): Var = x match {
    case n: BoundVarN => toBoundVar(n)
    case n: FreeVarN  => toFreeVar(n)
    case n: WildcardN => toWildcard(n)
    case _ =>
      assert(assertion = false, "Invalid Var type")
      Wildcard(WildcardMsg())
  }

  /** Unforgeable names */
  def toPrivate(x: UPrivateN): GPrivate = {
    val v = ByteString.copyFrom(x.v.toArray)
    GPrivate(v)
  }

  def toDeployId(x: UDeployIdN): GDeployId = {
    val v = ByteString.copyFrom(x.v.toArray)
    GDeployId(v)
  }

  def toDeployerId(x: UDeployerIdN): GDeployerId = {
    val v = ByteString.copyFrom(x.v.toArray)
    GDeployerId(v)
  }

  def toUnforgeable(x: UnforgeableN): GUnforgeable = x match {
    case n: UPrivateN    => toPrivate(n)
    case n: UDeployIdN   => toDeployId(n)
    case n: UDeployerIdN => toDeployerId(n)
    case _ =>
      assert(assertion = false, "Invalid Unforgeable type")
      GPrivate(ByteString.copyFromUtf8("42"))
  }

  /** Operations */
  def toENeg(x: ENegN): ENeg = {
    val p = toProto(x.p)
    ENeg(p)
  }

  def toENot(x: ENotN): ENot = {
    val p = toProto(x.p)
    ENot(p)
  }

  def toEPlus(x: EPlusN): EPlus = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EPlus(p1, p2)
  }

  def toEMinus(x: EMinusN): EMinus = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EMinus(p1, p2)
  }

  def toEMult(x: EMultN): EMult = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EMult(p1, p2)
  }

  def toEDiv(x: EDivN): EDiv = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EDiv(p1, p2)
  }

  def toEMod(x: EModN): EMod = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EMod(p1, p2)
  }

  def toELt(x: ELtN): ELt = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    ELt(p1, p2)
  }

  def toELte(x: ELteN): ELte = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    ELte(p1, p2)
  }

  def toEGt(x: EGtN): EGt = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EGt(p1, p2)
  }

  def toEGte(x: EGteN): EGte = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EGte(p1, p2)
  }

  def toEEq(x: EEqN): EEq = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EEq(p1, p2)
  }

  def toENeq(x: ENeqN): ENeq = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    ENeq(p1, p2)
  }

  def toEAnd(x: EAndN): EAnd = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EAnd(p1, p2)
  }

  def toEShortAnd(x: EShortAndN): EShortAnd = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EShortAnd(p1, p2)
  }

  def toEOr(x: EOrN): EOr = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EOr(p1, p2)
  }

  def toEShortOr(x: EShortOrN): EShortOr = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EShortOr(p1, p2)
  }

  def toEPlusPlus(x: EPlusPlusN): EPlusPlus = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EPlusPlus(p1, p2)
  }

  def toEMinusMinus(x: EMinusMinusN): EMinusMinus = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EMinusMinus(p1, p2)
  }

  def toEPercentPercent(x: EPercentPercentN): EPercentPercent = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EPercentPercent(p1, p2)
  }

  def toEMethod(x: EMethodN): EMethod = {
    val methodName     = x.methodName
    val target         = toProto(x.target)
    val arguments      = toProto(x.arguments)
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed
    EMethod(methodName, target, arguments, locallyFree, connectiveUsed)
  }

  def toEMatches(x: EMatchesN): EMatches = {
    val target  = toProto(x.target)
    val pattern = toProto(x.pattern)
    EMatches(target, pattern)
  }

  /** Connective */
  def toConnBool(x: ConnBoolN): ConnBool =
    ConnBool(true)

  def toConnInt(x: ConnIntN): ConnInt =
    ConnInt(true)

  def toConnBigInt(x: ConnBigIntN): ConnBigInt =
    ConnBigInt(true)

  def toConnString(x: ConnStringN): ConnString =
    ConnString(true)

  def toConnUri(x: ConnUriN): ConnUri =
    ConnUri(true)

  def toConnByteArray(x: ConnByteArrayN): ConnByteArray =
    ConnByteArray(true)

  def toConnNotBody(x: ConnNotN): ConnNotBody = {
    val p = toProto(x.p)
    ConnNotBody(p)
  }

  def toConnAndBody(x: ConnAndN): ConnAndBody = {
    val ps = ConnectiveBody(toProto(x.ps))
    ConnAndBody(ps)
  }

  def toConnOrBody(x: ConnOrN): ConnOrBody = {
    val ps = ConnectiveBody(toProto(x.ps))
    ConnOrBody(ps)
  }

  def toVarRefBody(x: ConnVarRefN): VarRefBody = {
    val index = x.index
    val depth = x.depth
    VarRefBody(VarRef(index, depth))
  }

  /** Other types */
  def toBundle(x: BundleN): Bundle = {
    val body      = toProto(x.body)
    val writeFlag = x.writeFlag
    val readFlag  = x.readFlag
    Bundle(body, writeFlag, readFlag)
  }

  def toGSysAuthToken(x: SysAuthTokenN): GSysAuthToken =
    GSysAuthToken()

}
