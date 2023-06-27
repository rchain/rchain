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

  behavior of "Par"

  /** Main types */
  it should "test ParProc" in {
    val p1 = ParProcN(Seq(GNilN(), ParProcN()))
    val p2 = ParProcN(Seq(ParProcN(), GNilN()))
    simpleCheck(p1, Some(p2)) should be(true)
  }

  it should "test Send with same data order" in {
    val p = SendN(GNilN(), Seq(GNilN(), SendN(GNilN(), GNilN())), persistent = true)
    simpleCheck(p) should be(true)
  }

  it should "test Send with different data order" in {
    val p1 = SendN(GNilN(), Seq(GNilN(), SendN(GNilN(), GNilN())), persistent = true)
    val p2 = SendN(GNilN(), Seq(SendN(GNilN(), GNilN()), GNilN()), persistent = true)
    simpleCheck(p1, Some(p2)) should be(false)
  }

  it should "test Receive with same data order" in {
    val bind1 = ReceiveBindN(Seq(FreeVarN(41), FreeVarN(42)), GNilN(), Some(BoundVarN(42)), 2)
    val bind2 = ReceiveBindN(Seq(FreeVarN(42), FreeVarN(41)), GNilN(), Some(BoundVarN(42)), 2)
    val p     = ReceiveN(Seq(bind1, bind2), GNilN(), persistent = true, peek = false, 4)
    simpleCheck(p) should be(true)
  }

  it should "test match with same data order" in {
    val case1 = MatchCaseN(FreeVarN(41), BoundVarN(42), 1)
    val case2 = MatchCaseN(WildcardN(), BoundVarN(42), 0)
    val p     = MatchN(GNilN(), Seq(case1, case2))
    simpleCheck(p) should be(true)
  }

  it should "test New" in {
    val p1 = NewN(1, BoundVarN(0), Seq("rho:io:stdout", "rho:io:stderr"))
    val p2 = NewN(1, BoundVarN(0), Seq("rho:io:stderr", "rho:io:stdout"))
    simpleCheck(p1, Some(p2)) should be(true)
  }

  /** Ground types */
  it should "test GNil" in {
    val p = GNilN()
    simpleCheck(p) should be(true)
  }

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

  it should "test GUri" in {
    val p = GUriN(strTest)
    simpleCheck(p) should be(true)
  }

  /** Collections */
  it should "test EList with same data order" in {
    val p = EListN(Seq(GNilN(), EListN()))
    simpleCheck(p) should be(true)
  }

  it should "test EList with different data order" in {
    val p1 = EListN(Seq(GNilN(), EListN()))
    val p2 = EListN(Seq(EListN(), GNilN()))
    simpleCheck(p1, Some(p2)) should be(false)
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

  /** Expr */
  /** Bundle */
  /** Connective */
  /** Other types */
  it should "test SysAuthToken" in {
    val p = SysAuthToken()
    simpleCheck(p) should be(true)
  }
}
