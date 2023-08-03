package coop.rchain.models.rholangn

import com.google.protobuf.ByteString
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models.Var.WildcardMsg
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.rholangn.Bindings._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.immutable.BitSet

class BindingsSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
  val sizeTest: Int          = 50
  val bytesTest: Array[Byte] = Array.fill(sizeTest)(42)
  val strTest: String        = List.fill(sizeTest)("42").mkString

  /** Basic types */
  it should "test Nil" in {
    val p1: ParN = NilN
    val p2: Par  = Par()
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ParProc" in {
    val p1: ParN = ParProcN(Seq(GIntN(42), GBoolN(true)))
    val p2: Par  = GInt(42) ++ GBool(true)
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "show error in old implementation" in {
    val p1: Par = Par(
      List(),
      List(),
      List(),
      Vector(Expr(GInt(42)), Expr(GBool(true))),
      List(),
      List(),
      List(),
      List(),
      AlwaysEqual(BitSet())
    )
    val p2: Par = Par(
      List(),
      List(),
      List(),
      Vector(Expr(GBool(true)), Expr(GInt(42))),
      List(),
      List(),
      List(),
      List(),
      AlwaysEqual(BitSet())
    )
    p1.equals(p2) should be(false)
  }

  it should "test Send" in {
    val p1: ParN = SendN(NilN, Seq(NilN, SendN(NilN, NilN)), persistent = true)
    val p2: Par  = Send(Par(), Seq(Par(), Send(Par(), Seq(Par()))), persistent = true)
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test Receive" in {
    val bind11   = ReceiveBindN(Seq(FreeVarN(41), FreeVarN(42)), NilN, Some(BoundVarN(42)), 2)
    val bind12   = ReceiveBindN(Seq(FreeVarN(42), FreeVarN(41)), NilN, Some(BoundVarN(42)), 2)
    val p1: ParN = ReceiveN(Seq(bind11, bind12), NilN, persistent = true, peek = false, 4)
    val bind21 =
      ReceiveBind(Seq(EVar(FreeVar(41)), EVar(FreeVar(42))), Par(), Some(BoundVar(42)), 2)
    val bind22 =
      ReceiveBind(Seq(EVar(FreeVar(42)), EVar(FreeVar(41))), Par(), Some(BoundVar(42)), 2)
    val p2: Par = Receive(Seq(bind21, bind22), Par(), persistent = true, peek = false, 4)
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test Match" in {
    val case11   = MatchCaseN(FreeVarN(41), BoundVarN(42), 1)
    val case12   = MatchCaseN(WildcardN, BoundVarN(42), 0)
    val p1: ParN = MatchN(NilN, Seq(case11, case12))
    val case21   = MatchCase(EVar(FreeVar(41)), EVar(BoundVar(42)), 1)
    val case22   = MatchCase(EVar(Wildcard(WildcardMsg())), EVar(BoundVar(42)))
    val p2: Par  = Match(Par(), Seq(case21, case22))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test New" in {
    val uri      = Seq("4", "2", "3", "1")
    val inj1     = Map("4" -> NilN, "3" -> NilN)
    val inj2     = Map("4" -> Par(), "3" -> Par())
    val p1: ParN = NewN(1, BoundVarN(0), uri, inj1)
    val p2: Par  = New(1, EVar(BoundVar(0)), uri, inj2)
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  /** Ground types */
  it should "test GBool" in {
    val p1: ParN = GBoolN(true)
    val p2: Par  = GBool(true)
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test GInt" in {
    val p1: ParN = GIntN(42)
    val p2: Par  = GInt(42)
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test GBigInt" in {
    val p1: ParN = GBigIntN(BigInt(bytesTest))
    val p2: Par  = GBigInt(BigInt(bytesTest))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test GString" in {
    val p1: ParN = GStringN(strTest)
    val p2: Par  = GString(strTest)
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test GByteArray" in {
    val p1: ParN = GByteArrayN(bytesTest)
    val p2: Par  = GByteArray(ByteString.copyFrom(bytesTest))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test GUri" in {
    val p1: ParN = GUriN(strTest)
    val p2: Par  = GUri(strTest)
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  /** Collections */
  it should "test EList" in {
    val p1: ParN = EListN(Seq(NilN, EListN()), Some(BoundVarN(42)))
    val p2: Par  = EList(Seq(Par(), EList()), BitSet(), connectiveUsed = false, Some(BoundVar(42)))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ETuple" in {
    val p1: ParN = ETupleN(Seq(NilN, ETupleN(NilN)))
    val p2: Par  = ETuple(Seq(Par(), ETuple(Seq(Par()))))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ESet" in {
    val p1: ParN = ESetN(Seq(NilN, ESetN()))
    val p2: Par  = ParSet(Seq(Par(), ParSet(Seq())))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EMap" in {
    val p1: ParN      = EMapN(Seq(NilN -> EMapN(), EMapN() -> NilN))
    val emptyMap: Par = ParMap(Seq())
    val p2: Par       = ParMap(Seq(Par() -> emptyMap, emptyMap -> Par()))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  /** Vars */
  it should "test BoundVar" in {
    val p1: ParN = BoundVarN(42)
    val p2: Par  = EVar(BoundVar(42))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test FreeVar" in {
    val p1: ParN = FreeVarN(42)
    val p2: Par  = EVar(FreeVar(42))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test Wildcard" in {
    val p1: ParN = WildcardN
    val p2: Par  = EVar(Wildcard(WildcardMsg()))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  /** Operations */
  it should "test ENeg" in {
    val p1: ParN = ENegN(GIntN(42))
    val p2: Par  = ENeg(GInt(42))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ENot" in {
    val p1: ParN = ENotN(GBoolN(true))
    val p2: Par  = ENot(GBool(true))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EPlus" in {
    val p1: ParN = EPlusN(GIntN(42), GIntN(43))
    val p2: Par  = EPlus(GInt(42), GInt(43))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EMinus" in {
    val p1: ParN = EMinusN(GIntN(42), GIntN(43))
    val p2: Par  = EMinus(GInt(42), GInt(43))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EMult" in {
    val p1: ParN = EMultN(GIntN(42), GIntN(43))
    val p2: Par  = EMult(GInt(42), GInt(43))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EDiv" in {
    val p1: ParN = EDivN(GIntN(42), GIntN(43))
    val p2: Par  = EDiv(GInt(42), GInt(43))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EMod" in {
    val p1: ParN = EModN(GIntN(42), GIntN(43))
    val p2: Par  = EMod(GInt(42), GInt(43))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ELt" in {
    val p1: ParN = ELtN(GIntN(42), GIntN(43))
    val p2: Par  = ELt(GInt(42), GInt(43))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ELte" in {
    val p1: ParN = ELteN(GIntN(42), GIntN(43))
    val p2: Par  = ELte(GInt(42), GInt(43))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EGt" in {
    val p1: ParN = EGtN(GIntN(42), GIntN(43))
    val p2: Par  = EGt(GInt(42), GInt(43))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EGteN" in {
    val p1: ParN = EGteN(GIntN(42), GIntN(43))
    val p2: Par  = EGte(GInt(42), GInt(43))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EEq" in {
    val p1: ParN = EEqN(GIntN(42), GIntN(43))
    val p2: Par  = EEq(GInt(42), GInt(43))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ENeq" in {
    val p1: ParN = ENeqN(GIntN(42), GIntN(43))
    val p2: Par  = ENeq(GInt(42), GInt(43))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EAnd" in {
    val p1: ParN = EAndN(GBoolN(true), GBoolN(false))
    val p2: Par  = EAnd(GBool(true), GBool(false))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EShortAnd" in {
    val p1: ParN = EShortAndN(GBoolN(true), GBoolN(false))
    val p2: Par  = EShortAnd(GBool(true), GBool(false))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EOr" in {
    val p1: ParN = EOrN(GBoolN(true), GBoolN(false))
    val p2: Par  = EOr(GBool(true), GBool(false))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EShortOr" in {
    val p1: ParN = EShortOrN(GBoolN(true), GBoolN(false))
    val p2: Par  = EShortOr(GBool(true), GBool(false))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EPlusPlus" in {
    val p1: ParN = EPlusPlusN(GStringN("42"), GStringN("43"))
    val p2: Par  = EPlusPlus(GString("42"), GString("43"))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EMinusMinus" in {
    val p1: ParN = EMinusMinusN(EListN(NilN), EListN(NilN))
    val p2: Par  = EMinusMinus(EList(Seq(Par())), EList(Seq(Par())))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EMatches" in {
    val p1: ParN = EMatchesN(GIntN(42), GIntN(42))
    val p2: Par  = EMatches(GInt(42), GInt(42))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EPercentPercent" in {
    val p1: ParN = EPercentPercentN(GStringN("x"), GIntN(42))
    val p2: Par  = EPercentPercent(GString("x"), GInt(42))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test EMethod" in {
    val p1: ParN = EMethodN("nth", EListN(NilN), GIntN(1))
    val p2: Par  = EMethod("nth", EList(Seq(Par())), Seq(GInt(1): Par))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  /** Unforgeable names */
  it should "test UPrivate" in {
    val p1: ParN = UPrivateN(bytesTest)
    val p2: Par  = GPrivate(ByteString.copyFrom(bytesTest))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test UDeployId" in {
    val p1: ParN = UDeployIdN(bytesTest)
    val p2: Par  = GDeployId(ByteString.copyFrom(bytesTest))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test UDeployerId" in {
    val p1: ParN = UDeployerIdN(bytesTest)
    val p2: Par  = GDeployerId(ByteString.copyFrom(bytesTest))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  /** Connective */
  it should "test ConnBool" in {
    val p1: ParN = ConnBoolN
    val p2: Par  = Connective(ConnBool(true))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ConnInt" in {
    val p1: ParN = ConnIntN
    val p2: Par  = Connective(ConnInt(true))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ConnBigInt" in {
    val p1: ParN = ConnBigIntN
    val p2: Par  = Connective(ConnBigInt(true))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ConnString" in {
    val p1: ParN = ConnStringN
    val p2: Par  = Connective(ConnString(true))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ConnUri" in {
    val p1: ParN = ConnUriN
    val p2: Par  = Connective(ConnUri(true))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ConnByteArray" in {
    val p1: ParN = ConnByteArrayN
    val p2: Par  = Connective(ConnByteArray(true))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ConnNotN" in {
    val p1: ParN = ConnNotN(SendN(NilN, NilN))
    val p2: Par  = Connective(ConnNotBody(Send(Par(), Seq(Par()))))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ConnAndN" in {
    val p1: ParN = ConnAndN(WildcardN, SendN(NilN, NilN))
    val p2: Par = Connective(
      ConnAndBody(ConnectiveBody(Seq(EVar(Wildcard(WildcardMsg())), Send(Par(), Seq(Par())))))
    )
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ConnOrN" in {
    val p1: ParN = ConnOrN(WildcardN, SendN(NilN, NilN))
    val p2: Par = Connective(
      ConnOrBody(ConnectiveBody(Seq(EVar(Wildcard(WildcardMsg())), Send(Par(), Seq(Par())))))
    )
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test ConnVarRefN" in {
    val p1: ParN = ConnVarRefN(0, 1)
    val p2: Par  = Connective(VarRefBody(VarRef(0, 1)))
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  /** Other types */
  it should "test Bundle" in {
    val p1: ParN = BundleN(NilN, writeFlag = true, readFlag = true)
    val p2: Par  = Bundle(Par(), writeFlag = true, readFlag = true)
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }

  it should "test SysAuthToken" in {
    val p1: ParN = USysAuthTokenN()
    val p2: Par  = GSysAuthToken()
    toProto(p1) should be(p2)
    fromProto(p2) should be(p1)
  }
}
