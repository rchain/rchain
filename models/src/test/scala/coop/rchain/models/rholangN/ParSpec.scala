package coop.rchain.models.rholangN

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {

  /** Test hashing and serialization for par
    * @param p1 Par for testing
    * @param p2Opt optional Par (used for testing if necessary to check the correct sorting)
    * @return true - if the result of serialization and hashing for both pairs is the same
    */
  def simpleCheck(p1: ParN, p2Opt: Option[ParN] = None): Boolean = {
    // Serialization and hashing testing
    val bytes1        = p1.toBytes
    val recover1      = ParN.fromBytes(bytes1)
    val res1: Boolean = p1.rhoHash == recover1.rhoHash

    // Testing possibility of calculating the rest of the metadata (without checking correctness)
    val _ = p1.connectiveUsed || p1.evalRequired || p1.substituteRequired

    // the correct sorting testing
    val res2: Boolean = if (p2Opt.isDefined) {
      val p2     = p2Opt.get
      val bytes2 = p2.toBytes
      (p1.rhoHash == p2.rhoHash) &&
      (bytes1 == bytes2) &&
      (p1.connectiveUsed == p2.connectiveUsed) &&
      (p1.evalRequired == p2.evalRequired) &&
      (p1.substituteRequired == p2.substituteRequired)
    } else true

    res1 && res2
  }

  val sizeTest: Int          = 50
  val bytesTest: Array[Byte] = Array.fill(sizeTest)(42)
  val strTest: String        = List.fill(sizeTest)("42").mkString

  /** Basic types */
  it should "test Nil" in {
    val p = NilN()
    simpleCheck(p) should be(true)
  }

  it should "test ParProc" in {
    val p1 = ParProcN(Seq(NilN(), ParProcN()))
    val p2 = ParProcN(Seq(ParProcN(), NilN()))
    simpleCheck(p1, Some(p2)) should be(true)
  }

  it should "test Send with same data order" in {
    val p = SendN(NilN(), Seq(NilN(), SendN(NilN(), NilN())), persistent = true)
    simpleCheck(p) should be(true)
  }

  it should "test Send with different data order" in {
    val p1 = SendN(NilN(), Seq(NilN(), SendN(NilN(), NilN())), persistent = true)
    val p2 = SendN(NilN(), Seq(SendN(NilN(), NilN()), NilN()), persistent = true)
    simpleCheck(p1, Some(p2)) should be(false)
  }

  it should "test Receive with same data order" in {
    val bind1 = ReceiveBindN(Seq(FreeVarN(41), FreeVarN(42)), NilN(), Some(BoundVarN(42)), 2)
    val bind2 = ReceiveBindN(Seq(FreeVarN(42), FreeVarN(41)), NilN(), Some(BoundVarN(42)), 2)
    val p     = ReceiveN(Seq(bind1, bind2), NilN(), persistent = true, peek = false, 4)
    simpleCheck(p) should be(true)
  }

  it should "test Receive with different data order" in {
    val bind1 = ReceiveBindN(Seq(FreeVarN(41), FreeVarN(42)), NilN(), Some(BoundVarN(42)), 2)
    val bind2 = ReceiveBindN(Seq(FreeVarN(42), FreeVarN(41)), NilN(), Some(BoundVarN(42)), 2)
    val p1    = ReceiveN(Seq(bind1, bind2), NilN(), persistent = true, peek = false, 4)
    val p2    = ReceiveN(Seq(bind2, bind1), NilN(), persistent = true, peek = false, 4)
    simpleCheck(p1, Some(p2)) should be(true)
  }

  it should "test match with same data order" in {
    val case1 = MatchCaseN(FreeVarN(41), BoundVarN(42), 1)
    val case2 = MatchCaseN(WildcardN(), BoundVarN(42), 0)
    val p     = MatchN(NilN(), Seq(case1, case2))
    simpleCheck(p) should be(true)
  }

  it should "test New with different data order" in {
    val inj1: Map[String, ParN] =
      Map("rho:rchain:deployId" -> NilN(), "rho:rchain:deployerId" -> NilN())
    val p1 = NewN(1, BoundVarN(0), Seq("rho:io:stdout", "rho:io:stderr"), inj1)

    val inj2: Map[String, ParN] =
      Map("rho:rchain:deployerId" -> NilN(), "rho:rchain:deployId" -> NilN())
    val p2 = NewN(1, BoundVarN(0), Seq("rho:io:stderr", "rho:io:stdout"), inj2)
    simpleCheck(p1, Some(p2)) should be(true)
  }

  /** Ground types */
  it should "test GBool" in {
    val p = GBoolN(true)
    simpleCheck(p) should be(true)
  }

  it should "test GInt" in {
    val p = GIntN(42)
    simpleCheck(p) should be(true)
  }

  it should "test GBigInt" in {
    val p = GBigIntN(BigInt(bytesTest))
    simpleCheck(p) should be(true)
  }

  it should "test GString" in {
    val p = GStringN(strTest)
    simpleCheck(p) should be(true)
  }

  it should "test GByteArray" in {
    val p = GByteArrayN(bytesTest)
    simpleCheck(p) should be(true)
  }

  it should "test GUri" in {
    val p = GUriN(strTest)
    simpleCheck(p) should be(true)
  }

  /** Collections */
  it should "test EList with same data order" in {
    val p = EListN(Seq(NilN(), EListN()), Some(BoundVarN(42)))
    simpleCheck(p) should be(true)
  }

  it should "test EList with different data order" in {
    val p1 = EListN(Seq(NilN(), EListN()), Some(BoundVarN(42)))
    val p2 = EListN(Seq(EListN(), NilN()), Some(BoundVarN(42)))
    simpleCheck(p1, Some(p2)) should be(false)
  }

  it should "test ETuple with same data order" in {
    val p = ETupleN(Seq(NilN(), ETupleN(NilN())))
    simpleCheck(p) should be(true)
  }

  it should "test ETuple with different data order" in {
    val p1 = ETupleN(Seq(NilN(), ETupleN(NilN())))
    val p2 = ETupleN(Seq(ETupleN(NilN()), NilN()))
    simpleCheck(p1, Some(p2)) should be(false)
  }

  it should "test ESet with same data order" in {
    val p = ESetN(Seq(NilN(), ESetN()), Some(BoundVarN(42)))
    simpleCheck(p) should be(true)
  }

  it should "test ESet with different data order" in {
    val p1 = ESetN(Seq(NilN(), ESetN(NilN())))
    val p2 = ESetN(Seq(ESetN(NilN()), NilN()))
    simpleCheck(p1, Some(p2)) should be(true)
  }

  it should "test EMap with same data order" in {
    val p = EMapN(Seq(NilN() -> EMapN(), EMapN() -> NilN()), Some(BoundVarN(42)))
    simpleCheck(p) should be(true)
  }

  it should "test EMap with different data order" in {
    val p1 = EMapN(Seq(NilN()  -> EMapN(), EMapN() -> NilN()))
    val p2 = EMapN(Seq(EMapN() -> NilN(), NilN()   -> EMapN()))
    simpleCheck(p1, Some(p2)) should be(true)
  }

  /** Vars */
  it should "test BoundVar" in {
    val p = BoundVarN(42)
    simpleCheck(p) should be(true)
  }

  it should "test FreeVar" in {
    val p = FreeVarN(42)
    simpleCheck(p) should be(true)
  }

  it should "test Wildcard" in {
    val p = WildcardN()
    simpleCheck(p) should be(true)
  }

  /** Operations */
  it should "test ENeg" in {
    val p = ENegN(GIntN(42))
    simpleCheck(p) should be(true)
  }

  it should "test ENot" in {
    val p = ENotN(GBoolN(true))
    simpleCheck(p) should be(true)
  }

  it should "test EPlus with same data order" in {
    val p = EPlusN(GIntN(42), GIntN(43))
    simpleCheck(p) should be(true)
  }

  it should "test EPlus with different data order" in {
    val p1 = EPlusN(GIntN(42), GIntN(43))
    val p2 = EPlusN(GIntN(43), GIntN(42))
    simpleCheck(p1, Some(p2)) should be(false)
  }

  it should "test EMinus" in {
    val p = EMinusN(GIntN(42), GIntN(43))
    simpleCheck(p) should be(true)
  }

  it should "test EMult" in {
    val p = EMultN(GIntN(42), GIntN(43))
    simpleCheck(p) should be(true)
  }

  it should "test EDiv" in {
    val p = EDivN(GIntN(42), GIntN(43))
    simpleCheck(p) should be(true)
  }

  it should "test EMod" in {
    val p = EModN(GIntN(42), GIntN(43))
    simpleCheck(p) should be(true)
  }

  it should "test ELt" in {
    val p = ELtN(GIntN(42), GIntN(43))
    simpleCheck(p) should be(true)
  }

  it should "test ELte" in {
    val p = ELteN(GIntN(42), GIntN(43))
    simpleCheck(p) should be(true)
  }

  it should "test EGt" in {
    val p = EGtN(GIntN(42), GIntN(43))
    simpleCheck(p) should be(true)
  }

  it should "test EGteN" in {
    val p = EGteN(GIntN(42), GIntN(43))
    simpleCheck(p) should be(true)
  }

  it should "test EEq with same data order" in {
    val p = EEqN(GIntN(42), GIntN(43))
    simpleCheck(p) should be(true)
  }

  it should "test ENeq" in {
    val p = ENeqN(GIntN(42), GIntN(43))
    simpleCheck(p) should be(true)
  }

  it should "test EAnd" in {
    val p = EAndN(GBoolN(true), GBoolN(false))
    simpleCheck(p) should be(true)
  }

  it should "test EShortAnd" in {
    val p = EShortAndN(GBoolN(true), GBoolN(false))
    simpleCheck(p) should be(true)
  }

  it should "test EOr" in {
    val p = EOrN(GBoolN(true), GBoolN(false))
    simpleCheck(p) should be(true)
  }

  it should "test EShortOr" in {
    val p = EShortOrN(GBoolN(true), GBoolN(false))
    simpleCheck(p) should be(true)
  }

  it should "test EPlusPlus" in {
    val p = EPlusPlusN(GStringN("42"), GStringN("43"))
    simpleCheck(p) should be(true)
  }

  it should "test EMinusMinus" in {
    val p = EMinusMinusN(EListN(NilN()), EListN(NilN()))
    simpleCheck(p) should be(true)
  }

  it should "test EMatches" in {
    val p = EMatchesN(GIntN(42), GIntN(42))
    simpleCheck(p) should be(true)
  }

  it should "test EPercentPercent" in {
    val p = EPercentPercentN(GStringN("x"), GIntN(42))
    simpleCheck(p) should be(true)
  }

  it should "test EMethod" in {
    val p = EMethodN("nth", EListN(NilN()), GIntN(1))
    simpleCheck(p) should be(true)
  }

  /** Unforgeable names */
  it should "test UPrivate" in {
    val p = UPrivateN(bytesTest)
    simpleCheck(p) should be(true)
  }

  it should "test UDeployId" in {
    val p = UDeployIdN(bytesTest)
    simpleCheck(p) should be(true)
  }

  it should "test UDeployerId" in {
    val p = UDeployerIdN(bytesTest)
    simpleCheck(p) should be(true)
  }

  /** Connective */
  it should "test ConnBool" in {
    val p = ConnBoolN()
    simpleCheck(p) should be(true)
  }

  it should "test ConnInt" in {
    val p = ConnIntN()
    simpleCheck(p) should be(true)
  }

  it should "test ConnBigInt" in {
    val p = ConnBigIntN()
    simpleCheck(p) should be(true)
  }

  it should "test ConnString" in {
    val p = ConnStringN()
    simpleCheck(p) should be(true)
  }

  it should "test ConnUri" in {
    val p = ConnUriN()
    simpleCheck(p) should be(true)
  }

  it should "test ConnByteArray" in {
    val p = ConnByteArrayN()
    simpleCheck(p) should be(true)
  }

  it should "test ConnNotN" in {
    val p = ConnNotN(SendN(NilN(), NilN()))
    simpleCheck(p) should be(true)
  }

  it should "test ConnAndN" in {
    val p = ConnAndN(WildcardN(), SendN(NilN(), NilN()))
    simpleCheck(p) should be(true)
  }

  it should "test ConnOrN" in {
    val p = ConnOrN(WildcardN(), SendN(NilN(), NilN()))
    simpleCheck(p) should be(true)
  }

  it should "test ConnVarRefN" in {
    val p = ConnVarRefN(0, 1)
    simpleCheck(p) should be(true)
  }

  /** Other types */
  it should "test Bundle" in {
    val p = BundleN(NilN(), writeFlag = true, readFlag = true)
    simpleCheck(p) should be(true)
  }

  it should "test SysAuthToken" in {
    val p = USysAuthTokenN()
    simpleCheck(p) should be(true)
  }
}
