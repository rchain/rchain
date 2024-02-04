package coop.rchain.models.rholangn

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

import scala.annotation.unused
import scala.collection.immutable.BitSet

private[rholangn] object BindingsToProto {
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def toProto(p: ParN): Par = p match {

    /** Basic types */
    case x: NilN.type => Par()
    case x: ParProcN  => toParProc(x)
    case x: SendN     => toSend(x)
    case x: ReceiveN  => toReceive(x)
    case x: MatchN    => toMatch(x)
    case x: NewN      => toNew(x)

    /** Expressions */
    case e: ExprN => toExpr(e)

    /** Unforgeable names */
    case u: UnforgeableN => toUnforgeable(u)

    /** Connective */
    case c: ConnectiveN => toConnective(c)

    /** Other types */
    case x: BundleN => toBundle(x)

    case _ => throw new Exception("Unknown type for toProto conversation")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def toExpr(e: ExprN): Expr = e match {

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
    case v: VarN => EVar(toVar(v))

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

    case _ => throw new Exception("Unknown type for Expression conversation")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def toVar(x: VarN): Var = x match {
    case n: BoundVarN      => toBoundVar(n)
    case n: FreeVarN       => toFreeVar(n)
    case _: WildcardN.type => Wildcard(WildcardMsg())
    case _                 => throw new Exception("Unknown type for Var conversation")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def toUnforgeable(u: UnforgeableN): GUnforgeable = u match {
    case x: UPrivateN      => toPrivate(x)
    case x: UDeployIdN     => toDeployId(x)
    case x: UDeployerIdN   => toDeployerId(x)
    case x: USysAuthTokenN => toGSysAuthToken(x)
    case _                 => throw new Exception("Unknown type for Unforgeable conversation")
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def toConnective(c: ConnectiveN): Connective = c match {
    case _: ConnBoolN.type      => Connective(ConnBool(true))
    case _: ConnIntN.type       => Connective(ConnInt(true))
    case _: ConnBigIntN.type    => Connective(ConnBigInt(true))
    case _: ConnStringN.type    => Connective(ConnString(true))
    case _: ConnUriN.type       => Connective(ConnUri(true))
    case _: ConnByteArrayN.type => Connective(ConnByteArray(true))
    case x: ConnNotN            => Connective(toConnNotBody(x))
    case x: ConnAndN            => Connective(toConnAndBody(x))
    case x: ConnOrN             => Connective(toConnOrBody(x))
    case x: ConnVarRefN         => Connective(toVarRefBody(x))
    case _                      => throw new Exception("Unknown type for Connective conversation")
  }

  private def toProto(ps: Seq[ParN]): Seq[Par]           = ps.map(toProto)
  private def toProto(varOpt: Option[VarN]): Option[Var] = varOpt.map(toVar)
  private def toProtoKVPairs(ps: Seq[(ParN, ParN)]): Seq[(Par, Par)] =
    ps.map { case (k, v) => (toProto(k), toProto(v)) }
  private def toProtoInjections(injections: Seq[(String, ParN)]): Seq[(String, Par)] =
    injections.map { case (str, p) => (str, toProto(p)) }

  /** Basic types */
  private def toParProc(x: ParProcN): Par = {
    val p = x.ps.foldLeft(Par())((acc, pN) => acc ++ toProto(pN))
    p.withConnectiveUsed(x.connectiveUsed.value)
  }

  def toSend(x: SendN): Send = {
    val chan           = toProto(x.chan)
    val data           = toProto(x.args)
    val persistent     = x.persistent
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed.value
    Send(chan, data, persistent, locallyFree, connectiveUsed)
  }

  def toReceive(x: ReceiveN): Receive = {
    val binds          = x.binds.map(toReceiveBind)
    val body           = toProto(x.body)
    val persistent     = x.persistent
    val peek           = x.peek
    val bindCount      = x.bindCount
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed.value
    Receive(binds, body, persistent, peek, bindCount, locallyFree, connectiveUsed)
  }

  private def toReceiveBind(x: ReceiveBindN): ReceiveBind = {
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
    val connectiveUsed = x.connectiveUsed.value
    Match(target, cases, locallyFree, connectiveUsed)
  }

  private def toMatchCase(x: MatchCaseN): MatchCase = {
    val pattern   = toProto(x.pattern)
    val source    = toProto(x.source)
    val freeCount = x.freeCount
    MatchCase(pattern, source, freeCount)
  }

  def toNew(x: NewN): New = {
    val bindCount                    = x.bindCount
    val p                            = toProto(x.p)
    val uri                          = x.uri
    val injections: Map[String, Par] = Map.from(toProtoInjections(x.injectionsStrKeys.toSeq))
    val locallyFree                  = BitSet()
    New(bindCount, p, uri.map(_.v), injections, locallyFree)
  }

  /** Ground types */
  private def toGBool(x: GBoolN): GBool = {
    val v = x.v
    GBool(v)
  }

  private def toGInt(x: GIntN): GInt = {
    val v = x.v
    GInt(v)
  }

  private def toGBigInt(x: GBigIntN): GBigInt = {
    val v = x.v
    GBigInt(v)
  }

  private def toGString(x: GStringN): GString = {
    val v = x.v
    GString(v)
  }

  private def toGByteArray(x: GByteArrayN): GByteArray = {
    val v = ByteString.copyFrom(x.v)
    GByteArray(v)
  }

  private def toGUri(x: GUriN): GUri = {
    val v = x.v
    GUri(v)
  }

  /** Collections */
  private def toEList(x: EListN): EList = {
    val ps             = toProto(x.ps)
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed.value
    val remainder      = toProto(x.remainder)
    EList(ps, locallyFree, connectiveUsed, remainder)
  }

  private def toETuple(x: ETupleN): ETuple = {
    val ps             = toProto(x.ps)
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed.value
    ETuple(ps, locallyFree, connectiveUsed)
  }

  private def toParSet(x: ESetN): ParSet = {
    val ps             = toProto(x.psSorted.value)
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed.value
    val remainder      = toProto(x.remainder)
    ParSet(ps, connectiveUsed, Sync[Eval].delay(locallyFree), remainder)
  }

  private def toParMap(x: EMapN): ParMap = {
    val ps             = toProtoKVPairs(x.psSorted.value)
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed.value
    val remainder      = toProto(x.remainder)
    ParMap(ps, connectiveUsed, Sync[Eval].delay(locallyFree), remainder)
  }

  /** Vars */
  private def toBoundVar(x: BoundVarN): BoundVar = {
    val idx = x.idx
    BoundVar(idx)
  }

  private def toFreeVar(x: FreeVarN): FreeVar = {
    val idx = x.idx
    FreeVar(idx)
  }

  /** Unforgeable names */
  private def toPrivate(x: UPrivateN): GPrivate = {
    val v = ByteString.copyFrom(x.v)
    GPrivate(v)
  }

  private def toDeployId(x: UDeployIdN): GDeployId = {
    val v = ByteString.copyFrom(x.v)
    GDeployId(v)
  }

  private def toDeployerId(x: UDeployerIdN): GDeployerId = {
    val v = ByteString.copyFrom(x.v)
    GDeployerId(v)
  }

  private def toGSysAuthToken(@unused x: USysAuthTokenN): GSysAuthToken =
    GSysAuthToken()

  /** Operations */
  private def toENeg(x: ENegN): ENeg = {
    val p = toProto(x.p)
    ENeg(p)
  }

  private def toENot(x: ENotN): ENot = {
    val p = toProto(x.p)
    ENot(p)
  }

  private def toEPlus(x: EPlusN): EPlus = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EPlus(p1, p2)
  }

  private def toEMinus(x: EMinusN): EMinus = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EMinus(p1, p2)
  }

  private def toEMult(x: EMultN): EMult = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EMult(p1, p2)
  }

  private def toEDiv(x: EDivN): EDiv = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EDiv(p1, p2)
  }

  private def toEMod(x: EModN): EMod = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EMod(p1, p2)
  }

  private def toELt(x: ELtN): ELt = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    ELt(p1, p2)
  }

  private def toELte(x: ELteN): ELte = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    ELte(p1, p2)
  }

  private def toEGt(x: EGtN): EGt = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EGt(p1, p2)
  }

  private def toEGte(x: EGteN): EGte = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EGte(p1, p2)
  }

  private def toEEq(x: EEqN): EEq = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EEq(p1, p2)
  }

  private def toENeq(x: ENeqN): ENeq = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    ENeq(p1, p2)
  }

  private def toEAnd(x: EAndN): EAnd = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EAnd(p1, p2)
  }

  private def toEShortAnd(x: EShortAndN): EShortAnd = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EShortAnd(p1, p2)
  }

  private def toEOr(x: EOrN): EOr = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EOr(p1, p2)
  }

  private def toEShortOr(x: EShortOrN): EShortOr = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EShortOr(p1, p2)
  }

  private def toEPlusPlus(x: EPlusPlusN): EPlusPlus = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EPlusPlus(p1, p2)
  }

  private def toEMinusMinus(x: EMinusMinusN): EMinusMinus = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EMinusMinus(p1, p2)
  }

  private def toEPercentPercent(x: EPercentPercentN): EPercentPercent = {
    val p1 = toProto(x.p1)
    val p2 = toProto(x.p2)
    EPercentPercent(p1, p2)
  }

  private def toEMethod(x: EMethodN): EMethod = {
    val methodName     = x.methodName
    val target         = toProto(x.target)
    val arguments      = toProto(x.args)
    val locallyFree    = BitSet()
    val connectiveUsed = x.connectiveUsed.value
    EMethod(methodName, target, arguments, locallyFree, connectiveUsed)
  }

  private def toEMatches(x: EMatchesN): EMatches = {
    val target  = toProto(x.target)
    val pattern = toProto(x.pattern)
    EMatches(target, pattern)
  }

  /** Connective */
  private def toConnNotBody(x: ConnNotN): ConnNotBody = {
    val p = toProto(x.p)
    ConnNotBody(p)
  }

  private def toConnAndBody(x: ConnAndN): ConnAndBody = {
    val ps = ConnectiveBody(toProto(x.ps))
    ConnAndBody(ps)
  }

  private def toConnOrBody(x: ConnOrN): ConnOrBody = {
    val ps = ConnectiveBody(toProto(x.ps))
    ConnOrBody(ps)
  }

  private def toVarRefBody(x: ConnVarRefN): VarRefBody = {
    val index = x.index
    val depth = x.depth
    VarRefBody(VarRef(index, depth))
  }

  /** Other types */
  private def toBundle(x: BundleN): Bundle = {
    val body      = toProto(x.body)
    val writeFlag = x.writeFlag
    val readFlag  = x.readFlag
    Bundle(body, writeFlag, readFlag)
  }
}
