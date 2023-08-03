package coop.rchain.models.rholangn

import coop.rchain.models.rholangn.CollectionSpecTestData._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

object CollectionSpecTestData {
  // After sorting, these two elements will be the same
  val pproc1: ParProcN = ParProcN(Seq(GIntN(42), NilN))
  val pproc2: ParProcN = ParProcN(Seq(NilN, GIntN(42)))
}

class EListSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
  it should "not preserve ordering" in {
    val p1 = EListN(Seq(NilN, EListN(), pproc1))
    val p2 = EListN(Seq(NilN, pproc1, EListN()))
    p1 should not be p2
  }

  it should "sort data in elements" in {
    val p1 = EListN(pproc1)
    val p2 = EListN(pproc2)
    p1 should be(p2)
  }

  it should "perform append operation" in {
    val p1 = EListN() :+ NilN :+ pproc1 :+ EListN()
    val p2 = EListN(Seq(NilN, pproc1, EListN()))
    p1 should be(p2)
  }

  it should "perform prepend operation" in {
    val p1 = NilN +: pproc1 +: EListN(EListN())
    val p2 = EListN(Seq(NilN, pproc1, ESetN()))
    p1 should be(p2)
  }

  it should "perform union operation" in {
    val p11 = EListN(Seq(pproc1, EListN()))
    val p12 = EListN(Seq(NilN, GIntN(42)))
    val p2  = EListN(Seq(pproc1, EListN(), NilN, GIntN(42)))
    p11 ++ p12 should be(p2)
  }

  it should "perform union with sequence operation" in {
    val p11 = EListN(Seq(pproc1, EListN()))
    val seq = Seq(NilN, GIntN(42))
    val p2  = EListN(Seq(pproc1, EListN(), NilN, GIntN(42)))
    p11 ++ seq should be(p2)
  }
}

class ETupleSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
  it should "throw exception during creation tuple with an empty par sequence " in {
    try {
      ETupleN(Seq())
    } catch {
      case ex: AssertionError => ex shouldBe a[AssertionError]
    }
  }
  it should "not preserve ordering" in {
    val p1 = ETupleN(Seq(NilN, ETupleN(NilN), pproc1))
    val p2 = ETupleN(Seq(NilN, pproc1, ETupleN(NilN)))
    p1 should not be p2
  }

  it should "sort data inside elements" in {
    val p1 = ESetN(pproc1)
    val p2 = ESetN(pproc2)
    p1 should be(p2)
  }
}

class ESetSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
  it should "preserve ordering" in {
    val p1 = ESetN(Seq(NilN, ESetN(), pproc1))
    val p2 = ESetN(Seq(NilN, pproc2, ESetN()))
    p1.sortedPs should be(p2.sortedPs)
    p1 should be(p2)
  }

  it should "deduplicate its elements where last seen element wins" in {
    val p1 = ESetN(Seq(NilN, ESetN(), pproc1, NilN, ESetN(), pproc2))
    val p2 = ESetN(Seq(NilN, ESetN(), pproc1))
    p1 should be(p2)
  }

  it should "distinguish different elements" in {
    val p1 = ESetN(Seq(GIntN(42), ESetN(), pproc1))
    val p2 = ESetN(Seq(GIntN(43), ESetN(), pproc1))
    p1 should not be p2
  }

  it should "perform append operation" in {
    val p1 = ESetN.empty + NilN + pproc1 + ESetN() + pproc2
    val p2 = ESetN(Seq(NilN, pproc1, ESetN()))
    p1 should be(p2)
  }

  it should "perform delete operation" in {
    val p1 = ESetN(Seq(NilN, pproc1, ESetN())) - pproc2 - ESetN() - GIntN(42)
    val p2 = ESetN(Seq(NilN))
    p1 should be(p2)
  }

  it should "perform contain operation" in {
    val p = ESetN(Seq(NilN, pproc1, ESetN()))
    p.contains(NilN) should be(true)
    p.contains(pproc2) should be(true)
    p.contains(GIntN(42)) should be(false)
  }

  it should "perform union operation" in {
    val p11 = ESetN(Seq(pproc1, ESetN()))
    val p12 = ESetN(Seq(NilN, pproc2, GIntN(42)))
    val p2  = ESetN(Seq(NilN, pproc1, ESetN(), GIntN(42)))
    p11 ++ p12 should be(p2)
  }

  it should "perform union operation with sequence" in {
    val p11 = ESetN(Seq(pproc1, ESetN()))
    val seq = Seq(NilN, pproc2, GIntN(42))
    val p2  = ESetN(Seq(NilN, pproc1, ESetN(), GIntN(42)))
    p11 ++ seq should be(p2)
  }

  it should "perform difference operation" in {
    val p1    = ESetN(Seq(NilN, pproc1, ESetN(), GIntN(42)))
    val p2    = ESetN(Seq(pproc1, ESetN(), GIntN(43)))
    val pDiff = ESetN(Seq(NilN, GIntN(42)))
    p1 -- p2 should be(pDiff)
  }

  it should "perform difference operation with sequence" in {
    val p1    = ESetN(Seq(NilN, pproc1, ESetN(), GIntN(42)))
    val seq   = Seq(pproc1, ESetN(), GIntN(43))
    val pDiff = ESetN(Seq(NilN, GIntN(42)))
    p1 -- seq should be(pDiff)
  }
}

class EMapSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
  it should "preserve ordering" in {
    val p1 = EMapN(Seq(NilN   -> GIntN(42), pproc1 -> EMapN()))
    val p2 = EMapN(Seq(pproc2 -> EMapN(), NilN     -> GIntN(42)))
    p1.sortedPs should be(p2.sortedPs)
    p1 should be(p2)
  }

  it should "deduplicate its elements where last seen element wins" in {
    val p1 =
      EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN(), NilN -> GIntN(43), pproc2 -> NilN))
    val p2 = EMapN(Seq(NilN -> GIntN(43), pproc1 -> NilN))
    p1 should be(p2)
  }

  it should "distinguish different elements" in {
    val p1 = EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN()))
    val p2 = EMapN(Seq(NilN -> GIntN(43), pproc1 -> EMapN()))
    p1 should not be p2
  }

  it should "perform append operation" in {
    val p1 = EMapN.empty +
      (NilN -> GIntN(42)) + (pproc1 -> GIntN(43)) + (EMapN() -> NilN) + (pproc2 -> EMapN())
    val p2 = EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN(), EMapN() -> NilN))
    p1 should be(p2)
  }

  it should "perform delete operation" in {
    val p1 = EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN(), EMapN() -> NilN)) -
      pproc2 - EMapN() - GIntN(42)
    val p2 = EMapN(Seq(NilN -> GIntN(42)))
    p1 should be(p2)
  }

  it should "perform union operation" in {
    val p11 = EMapN(Seq(NilN      -> GIntN(42), pproc1    -> EMapN()))
    val p12 = EMapN(Seq(GIntN(42) -> GIntN(43), pproc2    -> NilN))
    val p2  = EMapN(Seq(NilN      -> GIntN(42), GIntN(42) -> GIntN(43), pproc1 -> NilN))
    p11 ++ p12 should be(p2)
  }

  it should "perform union operation with sequence" in {
    val p11 = EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN()))
    val seq = Seq(GIntN(42) -> GIntN(43), pproc2 -> NilN)
    val p2  = EMapN(Seq(NilN -> GIntN(42), GIntN(42) -> GIntN(43), pproc1 -> NilN))
    p11 ++ seq should be(p2)
  }

  it should "perform difference operation" in {
    val p1    = EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN(), EMapN() -> NilN))
    val p2    = EMapN(Seq(NilN -> GIntN(42), pproc2 -> GIntN(42), EMapN() -> GIntN(42)))
    val pDiff = EMapN.empty
    p1 -- p2 should be(pDiff)
  }

  it should "perform difference operation with sequence" in {
    val p1    = EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN(), EMapN() -> NilN))
    val seq   = Seq(NilN, pproc2, EMapN())
    val pDiff = EMapN.empty
    p1 -- seq should be(pDiff)
  }

  it should "perform contain operation" in {
    val p = EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN()))
    p.contains(NilN) should be(true)
    p.contains(pproc2) should be(true)
    p.contains(GIntN(42)) should be(false)
  }

  it should "perform get() operation" in {
    val p = EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN()))
    p.get(NilN) should be(Some(GIntN(42)))
    p.get(pproc2) should be(Some(EMapN()))
    p.get(GIntN(42)) should be(None)
  }

  it should "perform getOrElse() operation" in {
    val p = EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN()))
    p.getOrElse(NilN, GIntN(43)) should be(GIntN(42))
    p.getOrElse(pproc2, GIntN(43)) should be(EMapN())
    p.getOrElse(GIntN(42), GIntN(43)) should be(GIntN(43))
  }

  it should "return keys in right order" in {
    val p     = EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN(), EMapN() -> NilN))
    val keys1 = p.keys
    val keys2 = p.sortedPs.map(_._1)
    keys1 should be(keys2)
  }

  it should "return values in right order" in {
    val p       = EMapN(Seq(NilN -> GIntN(42), pproc1 -> EMapN(), EMapN() -> NilN))
    val values1 = p.values
    val values2 = p.sortedPs.map(_._2)
    values1 should be(values2)
  }
}

class CollectionSortSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
  "ESet and EMap should " should "export pars in the same order as ParProc" in {
    val pProc = ParProcN(Seq(pproc1, ESetN(), GIntN(42), NilN))
    val set   = ESetN(Seq(pproc2, GIntN(42), ESetN(), NilN))
    val map   = EMapN(Seq(NilN -> NilN, pproc2 -> NilN, GIntN(42) -> NilN, ESetN() -> NilN))

    val ps1 = pProc.sortedPs
    val ps2 = set.sortedPs
    val ps3 = map.keys

    (ps1 == ps2) == (ps1 == ps3) should be(true)
  }
}
