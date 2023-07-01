package coop.rchain.models.rholangN

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SetSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {
  // After sorting, these two elements will be the same
  val pproc1: ParProcN = ParProcN(Seq(GIntN(42), NilN()))
  val pproc2: ParProcN = ParProcN(Seq(NilN(), GIntN(42)))

  it should "preserve ordering" in {
    val set1 = ESetN(Seq(NilN(), ESetN(), pproc1))
    val set2 = ESetN(Seq(NilN(), pproc2, ESetN()))
    set1.sortedPs should be(set2.sortedPs)
    set1 should be(set2)
  }

  it should "deduplicate its elements where last seen element wins" in {
    val set1 = ESetN(Seq(NilN(), ESetN(), pproc1, NilN(), ESetN(), pproc2))
    val set2 = ESetN(Seq(NilN(), ESetN(), pproc1))
    set1 should be(set2)
  }

  it should "distinguish different elements" in {
    val set1 = ESetN(Seq(GIntN(42), ESetN(), pproc1))
    val set2 = ESetN(Seq(GIntN(43), ESetN(), pproc1))
    set1 should not be set2
  }

  it should "perform append operation" in {
    val set1 = ESetN.empty + NilN() + pproc1 + ESetN() + pproc2
    val set2 = ESetN(Seq(NilN(), pproc1, ESetN()))
    set1 should be(set2)
  }

  it should "perform delete operation" in {
    val set1 = ESetN(Seq(NilN(), pproc1, ESetN())) - pproc2 - ESetN() - GIntN(42)
    val set2 = ESetN(Seq(NilN()))
    set1 should be(set2)
  }

  it should "perform contain operation" in {
    val set = ESetN(Seq(NilN(), pproc1, ESetN()))
    set.contains(NilN()) should be(true)
    set.contains(pproc2) should be(true)
    set.contains(GIntN(42)) should be(false)
  }

  it should "perform union operation" in {
    val set11 = ESetN(Seq(pproc1, ESetN()))
    val set12 = ESetN(Seq(NilN(), pproc2, GIntN(42)))
    val set2  = ESetN(Seq(NilN(), pproc1, ESetN(), GIntN(42)))
    set11.union(set12) should be(set2)
  }
}
