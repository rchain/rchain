package coop.rchain.rspace.merging

import coop.rchain.rspace.merger.MergingLogic._
import coop.rchain.shared.Stopwatch
import org.scalatest.{FlatSpec, Matchers}

class MergingLogic extends FlatSpec with Matchers {
  // some random conflict maps and rejection options, computed manually
  "rejection options" should "be computed correctly" in {
    computeRejectionOptions(
      Map(
        1 -> Set(2, 3, 4),
        2 -> Set(1),
        3 -> Set(1, 2),
        4 -> Set(1)
      )
    ) shouldBe Set(Set(1, 2), Set(2, 3, 4))

    computeRejectionOptions(
      Map(
        1 -> Set(2, 3, 4),
        2 -> Set(1, 3, 4),
        3 -> Set(1, 2, 4),
        4 -> Set(1, 2, 3)
      )
    ) shouldBe Set(Set(2, 3, 4), Set(1, 3, 4), Set(1, 2, 4), Set(1, 2, 3))

    computeRejectionOptions(
      Map(
        1 -> Set(2, 3, 4),
        2 -> Set(1),
        3 -> Set(1, 4),
        4 -> Set(1, 3)
      )
    ) shouldBe Set(Set(2, 3, 4), Set(1, 3), Set(1, 4))

    computeRejectionOptions(
      Map(
        1 -> Set.empty[Int],
        2 -> Set(3),
        3 -> Set(2, 4),
        4 -> Set(3)
      )
    ) shouldBe Set(Set(3), Set(2, 4))

    val all         = (1 to 1000) toSet
    val conflictMap = (1 to 1000).map(i => i -> (all - i)).toMap
    computeRejectionOptions(conflictMap) shouldBe (1 to 1000).map(all - _).toSet
  }
}
