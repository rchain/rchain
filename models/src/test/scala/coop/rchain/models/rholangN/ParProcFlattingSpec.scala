package coop.rchain.models.rholangN

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParProcFlattingSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {

  it should "test flatting empty data" in {
    val p        = ParN.makeParProc(Seq())
    val expected = NilN()
    p should be(expected)
  }

  it should "test flatting single Nil data" in {
    val p        = ParN.makeParProc(Seq(NilN()))
    val expected = NilN()
    p should be(expected)
  }

  it should "test flatting single not Nil data" in {
    val p        = ParN.makeParProc(Seq(GIntN(42)))
    val expected = GIntN(42)
    p should be(expected)
  }

  it should "test flatting multiple data" in {
    val p        = ParN.makeParProc(Seq(GIntN(42), GIntN(43)))
    val expected = ParProcN(Seq(GIntN(42), GIntN(43)))
    p should be(expected)
  }

  it should "test flatting multiple same data" in {
    val p        = ParN.makeParProc(Seq(GIntN(42), GIntN(42)))
    val expected = ParProcN(Seq(GIntN(42), GIntN(42)))
    p should be(expected)
  }

  it should "test flatting multiple data with Nil" in {
    val p        = ParN.makeParProc(Seq(GIntN(42), GIntN(43), NilN()))
    val expected = ParProcN(Seq(GIntN(42), GIntN(43)))
    p should be(expected)
  }

  it should "test flatting 2 data with Nil" in {
    val p        = ParN.makeParProc(Seq(GIntN(42), NilN()))
    val expected = GIntN(42)
    p should be(expected)
  }

  it should "test flatting nested data" in {
    val pProc1   = ParProcN(Seq(GIntN(42), GIntN(43)))
    val pProc2   = ParProcN(Seq(GIntN(44), GIntN(45)))
    val p        = ParN.makeParProc(Seq(pProc1, pProc2))
    val expected = ParProcN(Seq(GIntN(42), GIntN(43), GIntN(44), GIntN(45)))
    p should be(expected)
  }

  it should "test flatting nested single data" in {
    val pProc1   = ParProcN(Seq(GIntN(42)))
    val pProc2   = ParProcN(Seq(NilN()))
    val p        = ParN.makeParProc(Seq(pProc1, pProc2))
    val expected = GIntN(42)
    p should be(expected)
  }
}
