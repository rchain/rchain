package coop.rchain.models.testUtils
import coop.rchain.models.Par
import coop.rchain.models.rholang.sorter.Sortable
import monix.eval.Coeval
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Assertion
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks._

object TestUtils {
  def sort(par: Par): Par                                            = Sortable[Par].sortMatch[Coeval](par).map(_.term).value()
  def forAllSimilarA[A: Arbitrary](block: (A, A) => Assertion): Unit =
    // ScalaCheck generates similar A-s in subsequent calls which
    // we need to hit the case where `x == y` more often
    forAll(Gen.listOfN(5, arbitrary[A])) { as: List[A] =>
      for (x :: y :: Nil <- as.combinations(2)) {
        block(x, y)
      }
    }
}
