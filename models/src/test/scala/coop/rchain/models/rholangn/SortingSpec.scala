package coop.rchain.models.rholangn

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SortingSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
  @SuppressWarnings(Array("org.wartremover.warts.Return", "org.wartremover.warts.Var"))
  def compareHashes(a: Array[Byte], b: Array[Byte]): Int =
    if (a eq null) {
      if (b eq null) 0
      else -1
    } else if (b eq null) 1
    else {
      val L = math.min(a.length, b.length)
      var i = 0
      while (i < L) {
        if (a(i) < b(i)) return -1
        else if (b(i) < a(i)) return 1
        i += 1
      }
      if (L < b.length) -1
      else if (L < a.length) 1
      else 0
    }

  it should "test sorting for ParProc" in {
    val unsorted: Seq[GIntN] = Seq(GIntN(2), GIntN(5), GIntN(1), GIntN(3), GIntN(4), GIntN(2))
    val sorted               = ParProcN(unsorted).psSorted.value
    val expected: Seq[GIntN] =
      unsorted.sortWith((a, b) => compareHashes(a.rhoHash.value, b.rhoHash.value) < 0)
    sorted should be(expected)
  }

  it should "test sorting for ESet" in {
    val unsorted: Seq[GIntN] = Seq(GIntN(2), GIntN(5), GIntN(1), GIntN(3), GIntN(4))
    val sorted               = ESetN(unsorted).psSorted.value
    val expected: Seq[GIntN] =
      unsorted.sortWith((a, b) => compareHashes(a.rhoHash.value, b.rhoHash.value) < 0)
    sorted should be(expected.distinct)
  }

  it should "test sorting for EMap>" in {
    val unsorted: Seq[GIntN] = Seq(GIntN(2), GIntN(5), GIntN(1), GIntN(3), GIntN(4))
    val values               = Seq.range(1, unsorted.length + 1).map(x => GIntN(x.toLong))
    val pars                 = unsorted zip values
    val sorted               = EMapN(pars).psSorted.value
    val expectedPars =
      pars.sortWith((a, b) => compareHashes(a._1.rhoHash.value, b._1.rhoHash.value) < 0)
    sorted should be(expectedPars)
  }

  it should "test sorting for receive binds" in {
    val bind1         = ReceiveBindN(Seq(FreeVarN(41)), NilN, Some(BoundVarN(42)), 1)
    val bind2         = ReceiveBindN(Seq(FreeVarN(42)), NilN, Some(BoundVarN(42)), 1)
    val bind3         = ReceiveBindN(Seq(FreeVarN(43)), NilN, Some(BoundVarN(42)), 1)
    val bind4         = ReceiveBindN(Seq(FreeVarN(44)), NilN, Some(BoundVarN(42)), 1)
    val bind5         = ReceiveBindN(Seq(FreeVarN(45)), NilN, Some(BoundVarN(42)), 1)
    val unsortedBinds = Seq(bind1, bind2, bind3, bind4, bind5)
    val receive       = ReceiveN(unsortedBinds, NilN, 0)

    val sorted = receive.bindsSorted.value
    val expected =
      unsortedBinds.sortWith((a, b) => compareHashes(a.rhoHash.value, b.rhoHash.value) < 0)
    sorted should be(expected)
  }

}
