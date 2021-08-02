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

    val all         = (1 to 1000) toSet
    val conflictMap = (1 to 1000).map(i => i -> (all - i)).toMap
    computeRejectionOptions(conflictMap) shouldBe (1 to 1000).map(all - _).toSet
  }

  "benchmark" should "" ignore {
    val conflictSetSizes = Range(10, 100, 5)
    conflictSetSizes.map { conflictSetSize =>
      val fullPairs = (1 to conflictSetSize).combinations(2).toSet
      // One third random pairs are conflicting
      val conflictingPairs = fullPairs.take(fullPairs.size / 3)
      val conflictMap = conflictingPairs.foldLeft(Map.empty[Int, Set[Int]]) {
        case (acc, Seq(l, r)) =>
          acc +
            (l -> (acc.getOrElse(l, Set.empty) + r)) +
            (r -> (acc.getOrElse(r, Set.empty) + l))
      }
      val (_, time) = Stopwatch.profile(computeRejectionOptions(conflictMap))
      println(s"$conflictSetSize in ${time}")
    }
  }
}
