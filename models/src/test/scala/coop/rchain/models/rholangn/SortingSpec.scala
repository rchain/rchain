package coop.rchain.models.rholangn

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SortingSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
  it should "test sorting for receive binds" in {
    val bind1    = ReceiveBindN(Seq(FreeVarN(41)), NilN, Some(BoundVarN(42)), 1)
    val bind2    = ReceiveBindN(Seq(FreeVarN(42)), NilN, Some(BoundVarN(42)), 1)
    val bind3    = ReceiveBindN(Seq(FreeVarN(43)), NilN, Some(BoundVarN(42)), 1)
    val bind4    = ReceiveBindN(Seq(FreeVarN(44)), NilN, Some(BoundVarN(42)), 1)
    val bind5    = ReceiveBindN(Seq(FreeVarN(45)), NilN, Some(BoundVarN(42)), 1)
    val unsorted = Seq(bind1, bind2, bind3, bind4, bind5)
    val sorted   = parmanager.Manager.sortBinds(unsorted)
    val expected = Seq(bind3, bind2, bind1, bind4, bind5)
    sorted should be(expected)

    val bind1WithT    = (bind1, 1)
    val bind2WithT    = (bind2, 2)
    val bind3WithT    = (bind3, 3)
    val bind4WithT    = (bind4, 4)
    val bind5WithT    = (bind5, 5)
    val unsortedWithT = Seq(bind1WithT, bind2WithT, bind3WithT, bind4WithT, bind5WithT)
    val sortedWithT   = parmanager.Manager.sortBindsWithT(unsortedWithT)
    val expectedWithT = Seq(bind3WithT, bind2WithT, bind1WithT, bind4WithT, bind5WithT)
    sortedWithT should be(expectedWithT)
  }
}
