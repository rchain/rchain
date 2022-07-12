package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.MergingBenchmarkSpec.random
import coop.rchain.casper.merging.{DeployChainIndex, DeployIdWithCost}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.merger.{EventLogIndex, StateChange}
import coop.rchain.sdk.dag.merging.DagMergingLogic.computeRejectionOptions
import coop.rchain.shared.Stopwatch
import org.scalatest.flatspec.AnyFlatSpec

import java.util.Objects
import scala.util.Random

class MergingBenchmarkSpec extends AnyFlatSpec {
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
      val dciFullPairs = random.take(conflictSetSize).toList.combinations(2).toSet
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
object MergingBenchmarkSpec {
  def random: Iterator[DeployChainIndex] =
    Iterator.continually[Int](Random.nextInt(10) + 1).map { size =>
      val deployIds = Range(0, size).map { _ =>
        ByteString.copyFrom(Array.fill(64)((scala.util.Random.nextInt(256) - 128).toByte))
      }
      DeployChainIndex(
        Blake2b256Hash.fromByteArray(new Array[Byte](32)),
        deployIds.map(id => DeployIdWithCost(id, 0)).toSet,
        Blake2b256Hash.fromByteArray(new Array[Byte](32)),
        Blake2b256Hash.fromByteArray(new Array[Byte](32)),
        EventLogIndex.empty,
        StateChange.empty,
        Objects.hash(deployIds.map(id => DeployIdWithCost(id, 0)).map(_.id): _*)
      )
    }
}
