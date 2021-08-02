package coop.rchain.rspace.merging

import cats.effect.Sync
import coop.rchain.rspace.merger.MergingLogic._
import coop.rchain.shared.Stopwatch
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

class MergingLogic extends FlatSpec with Matchers {
//  implicit val s = Sync[Task]

  "1" should "" in {
    val conflictMap = Map(
      1 -> Set(2, 3, 4),
      2 -> Set(1),
      3 -> Set(1, 2),
      4 -> Set(1)
    )
    val rjOptions = computeRejectionOptions(conflictMap)
    rjOptions shouldBe Set(Set(1, 2), Set(2, 3, 4))
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

  "5" should "" in {
    val conflictSetSizes = Range(10, 100, 5)
    conflictSetSizes.map { conflictSetSize =>
      val fullPairs        = (1 to conflictSetSize).combinations(2).toSet
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
