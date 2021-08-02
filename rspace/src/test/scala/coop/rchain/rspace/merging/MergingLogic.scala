package coop.rchain.rspace.merging

import coop.rchain.rspace.merger.MergingLogic._
import org.scalatest.{FlatSpec, Matchers}

class MergingLogic extends FlatSpec with Matchers {
  "1" should "" in {
    val conflictMap = Map(
      1 -> Set(2, 3, 4),
      2 -> Set(1),
      3 -> Set(1, 2),
      4 -> Set(1)
    )
    val rjOptions = computeRejectionOptions(conflictMap)
    rjOptions shouldBe Set(Set(1), Set(1, 2), Set(2, 3, 4))
  }

  "2" should "" in {
    val conflictMap = Map(
      1 -> Set(2, 3, 4),
      2 -> Set(1, 3, 4),
      3 -> Set(1, 2, 4),
      4 -> Set(1, 2, 3)
    )
    val rjOptions = computeRejectionOptions(conflictMap)
    rjOptions shouldBe Set(Set(2, 3, 4), Set(1, 3, 4), Set(1, 2, 4), Set(1, 2, 3))
  }

  "3" should "" in {
    val conflictMap = Map(
      1 -> Set(2, 3, 4),
      2 -> Set(1),
      3 -> Set(1, 4),
      4 -> Set(1, 3)
    )
    val rjOptions = computeRejectionOptions(conflictMap)
    rjOptions shouldBe Set(Set(2, 3, 4), Set(1, 3), Set(1, 4))
  }

  "4" should "" in {
    val all         = (1 to 1000) toSet
    val conflictMap = (1 to 1000).map(i => i -> (all - i)).toMap
    val rjOptions   = computeRejectionOptions(conflictMap)

    val reference = (1 to 1000).map(all - _).toSet
    rjOptions shouldBe reference
  }
}
