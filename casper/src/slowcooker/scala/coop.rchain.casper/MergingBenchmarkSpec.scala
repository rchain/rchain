package coop.rchain.casper

import coop.rchain.casper.merging.DeployChainIndex
import coop.rchain.rspace.merger.MergingLogic.computeRejectionOptions
import coop.rchain.shared.Stopwatch
import org.scalatest.FlatSpec

class MergingBenchmarkSpec extends FlatSpec {
  "rejections option benchmark" should "for DeplyChainIndex" in {
    def conflictsMap[A](conflictingPairs: Set[List[A]]): Map[A, Set[A]] =
      conflictingPairs.foldLeft(Map.empty[A, Set[A]]) {
        case (acc, Seq(l, r)) =>
          acc +
            (l -> (acc.getOrElse(l, Set.empty) + r)) +
            (r -> (acc.getOrElse(r, Set.empty) + l))
      }

    val conflictSetSizes = Range(5, 100, 5)
    conflictSetSizes.map { conflictSetSize =>
      val dciFullPairs = DeployChainIndex.random.take(conflictSetSize).toList.combinations(2).toSet
      val intFullPairs = (1 to conflictSetSize).toList.combinations(2).toSet

      // One third random pairs are conflicting
      val dciConflictingPairs = dciFullPairs.take(dciFullPairs.size / 3)
      val intConflictingPairs = intFullPairs.take(intFullPairs.size / 3)

      val (_, dciTime) =
        Stopwatch.profile(computeRejectionOptions(conflictsMap(dciConflictingPairs)))
      val (_, intTime) =
        Stopwatch.profile(computeRejectionOptions(conflictsMap(intConflictingPairs)))
      println(
        s"Conflict set size ${conflictSetSize}, 1/3 pairs conflicting. " +
          s"Rejection options computed in: for Int (fastest possible): ${intTime}, for DeployChainIndex: ${dciTime} }"
      )
    }
  }
}
