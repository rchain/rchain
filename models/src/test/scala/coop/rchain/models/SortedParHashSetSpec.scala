package coop.rchain.models

import java.util

import com.google.protobuf.ByteString
import coop.rchain.models.Assertions.assertEqual
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance.BoundVar
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.rholang.sorter.Sortable
import coop.rchain.models.rholang.sorter.ordering._
import coop.rchain.models.testImplicits._
import coop.rchain.models.testUtils.TestUtils.sort
import monix.eval.Coeval
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.collection.immutable.BitSet

class SortedParHashSetSpec extends FlatSpec with ScalaCheckPropertyChecks with Matchers {

  val pars: Seq[Par] = {
    val parGround =
      Par().withExprs(Seq(GInt(2), GInt(1), GInt(-1), GInt(-2), GInt(0)))
    val parExpr: Par =
      EPlus(EPlus(GInt(1), GInt(3)), GInt(2))
    val parMethods =
      Par().withExprs(
        List(
          EMethod("nth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2)),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2)),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(2), GInt(3)), locallyFree = BitSet(2)),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(2)), locallyFree = BitSet(2))
        )
      )
    Seq(parGround, parExpr, parMethods)
  }

  //TODO(mateusz.gorski): Once we handle `Empty` cases from scalapb use scalacheck generators
  def sample = SortedParHashSet(pars)

  private[this] def roundtripTest(parTreeSet: SortedParHashSet): Assertion =
    ESet.parseFrom(serializeESet(parTreeSet)) should ===(ESet(ps = parTreeSet.sortedPars))

  // toByteArray method is using the same method calls as real protobuf's serialization
  private[this] def serializeESet(parTreeSet: SortedParHashSet): Array[Byte] =
    ESet(ps = parTreeSet.sortedPars).toByteArray

  "SortedParHashSet" should "preserve structure during round trip protobuf serialization" in {
    roundtripTest(sample)
  }

  it should "preserve ordering during serialization (required for deterministic serialization)" in {
    val referenceBytes = serializeESet(sample)

    // just to make sure that it's not a fluke, serialize it 1000 times
    (1 to 1000).forall { i =>
      withClue(s"Run #$i serialization: ") {
        val serialized = serializeESet(sample)
        val res        = util.Arrays.equals(serialized, referenceBytes)
        assert(res == true, ". Same set of Pars should serialize deterministically")
        res
      }
    } should be(true)
  }

  it should "deduplicate its elements where last seen element wins" in {
    def deduplicate(in: Seq[Par]): Set[Par] =
      in.foldLeft(Set.empty[Par])(_ + _)

    val elements: Seq[Par] = Seq(
      GInt(1),
      GInt(1),
      GBool(true),
      GBool(true),
      GBool(false),
      EMethod("nth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2)),
      EMethod("nth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2))
    )

    val expected = deduplicate(elements)

    val shs = SortedParHashSet(elements)

    shs.sortedPars should contain theSameElementsAs expected
  }

  it should "be equal when it is equal" in {
    val elements: Seq[Par] = Seq(
      Par(
        unforgeables = Seq(
          GPrivate(
            ByteString.copyFrom(
              Array[Byte](
                0
              )
            )
          ),
          GPrivate()
        ),
        connectiveUsed = true
      )
    )
    assertEqual(SortedParHashSet(elements), SortedParHashSet(elements))
  }

  it should "sort all input" in {
    forAll { pars: Seq[Par] =>
      val set = SortedParHashSet(pars)

      pars.headOption.foreach((unsorted: Par) => {
        val sorted = sort(unsorted)
        checkSortedInput(set.+, unsorted, sorted)
        checkSortedInput(set.-, unsorted, sorted)
        checkSortedInput(set.contains, unsorted, sorted)
        checkSortedInput(set.union, pars.toSet, pars.toList.sort.toSet)
      })
    }
  }

  it should "preserve sortedness of all elements and the whole map for all its operations" in {
    forAll { pars: Seq[Par] =>
      val set = SortedParHashSet(pars)
      checkSorted(set)

      set.headOption.foreach(par => {
        checkSorted(set + par)
        checkSorted(set - par)
        checkSorted(set.union(Set(par)))
      })
    }
  }

  private def checkSorted(iterable: Iterable[Par]) = {
    import coop.rchain.models.rholang.sorter.ordering._
    iterable.foreach(p => assert(isSorted(p)))
    assert(iterable.toList == iterable.toList.sort)
  }

  def isSorted[A: Sortable](a: A): Boolean =
    a == Sortable[A].sortMatch[Coeval](a).value().term

  private def checkSortedInput[A, B](f: A => B, unsorted: A, sorted: A): Assertion =
    assert(f(sorted) == f(unsorted))
}
