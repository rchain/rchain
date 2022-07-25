package coop.rchain.sdk.merging
import coop.rchain.sdk.dag.merging.DagMergingLogic._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.Checkers
import cats.syntax.all._

class DagMergingLogicSpec extends AnyFlatSpec with Matchers with Checkers {
  "withDependencies" should "emit element and all dependencies." in {
    val dependentsMap = Map(1 -> Set(3, 9), 3 -> Set(5), 5 -> Set(6), 4 -> Set(6))
    val rejects       = withDependencies(Set(1), dependentsMap)
    rejects shouldBe Set(1, 3, 9, 5, 6)
  }

  "incompatibleWithFinal" should "output conflicting with finally accepted or depending on finally rejected." in {
    val acceptedFinally = Set(1, 2)
    val rejectedFinally = Set(5, 6)
    val conflictsMap    = Map(1 -> Set(11, 12), 2 -> Set(21, 22), 3 -> Set(31, 32))
    val dependencyMap   = Map(5 -> Set(51, 52), 6 -> Set(61, 62), 7 -> Set(71, 72))
    val r               = incompatibleWithFinal(acceptedFinally, rejectedFinally, conflictsMap, dependencyMap)
    r shouldBe Set(11, 12, 21, 22, 51, 52, 61, 62)
  }

  "partitionScope" should "output non intersecting partitions" in {
    val views = Seq(Set(1, 2, 3, 4), Set(4, 5, 6, 7), Set(7, 8, 9), Set(9, 10))
    val r     = partitionScope(views)
    r shouldBe Seq(Set(1, 2, 3, 4), Set(5, 6, 7), Set(8, 9), Set(10))
  }

  "computeConflictsMap" should "compute relation map with bi directional relations." in {
    val set                       = Set(1, 2, 3, 4, 5, 6)
    val conflictsMap              = Map(1 -> Set(2, 3), 4 -> Set(5), 6 -> Set(6))
    val mirror                    = conflictsMap.map { case (k, v) => v.map(_ -> Set(k)) }.toList.combineAll
    val reference                 = conflictsMap ++ mirror
    def conflicts(a: Int, b: Int) = reference.get(a).exists(_.contains(b))
    computeConflictsMap(set, set, conflicts) shouldBe reference - 6 // should not contain self conflicts
  }

  "computeDependencyMap" should "compute correct map. Keys should be dependencies." in {
    val set           = Set(1, 2, 3, 4, 5, 6)
    val dependencyMap = Map(1 -> Set(2, 3), 4 -> Set(5), 3 -> Set(1), 6 -> Set(6))
    def depends(target: Int, maybeDependency: Int) =
      dependencyMap.get(maybeDependency).exists(_.contains(target))
    computeDependencyMap(set, set, depends) shouldBe dependencyMap - 6 // should not contain self depends
  }

  "computeBranches" should "compute branches that cover all target and all tips/roots are concurrent." in {
    val set           = Set(1, 2, 3, 4, 5, 6, 7, 100, 101)
    val dependencyMap = Map(1 -> Set(4, 5), 4 -> Set(5, 6), 2 -> Set(4, 5, 6), 3 -> Set(6, 7))
    computeBranches(set, dependencyMap) shouldBe Map(
      1   -> Set(4, 5, 6),
      2   -> Set(4, 5, 6),
      3   -> Set(6, 7),
      100 -> Set.empty[Int],
      101 -> Set.empty[Int]
    )
  }

  "computeRelationMapForMergeSet" should "output map with " +
    "- conflicts inside conflict set" +
    "- conflicts between conflict set and final set" +
    "- dependencies between conflict set and final set." in {
    val conflictSet  = Set(1, 2)
    val finalSet     = Set(3, 4)
    val conflictsMap = Map(1 -> Set(2), 2 -> Set(1))
    val dependsMap   = Map(3 -> Set(2), 10 -> Set(9))
    def conflicts(a: Int, b: Int) =
      conflictsMap.get(a).exists(_.contains(b))
    def depends(target: Int, maybeDependency: Int) =
      dependsMap.get(maybeDependency).exists(_.contains(target))
    computeRelationMapForMergeSet(conflictSet, finalSet, conflicts, depends) shouldBe
      (conflictsMap, dependsMap.filterKeys(finalSet.contains))
  }

  // some random conflict maps and rejection options, computed manually
  "rejection options" should "be computed correctly." in {
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

    val all          = (1 to 1000) toSet
    val conflictsMap = (1 to 1000).map(i => i -> (all - i)).toMap
    computeRejectionOptions(conflictsMap) shouldBe (1 to 1000).map(all - _).toSet
  }

  "computeOptimalRejection" should "pick rejection with minimal cost. " +
    "Equal cost should be resolved by size and the whole set." in {
    val rejectionOptions = Set(Set(1, 2, 3), Set(2, 3, 4), Set(1, 2), Set(2), Set(1))
    val cost             = Set(1, 2, 3, 4).map(_ -> 1L).toMap
    computeOptimalRejection(rejectionOptions, cost(_: Int)) shouldBe Set(1)
  }

  "addMergeableOverflowRejections" should "fold mergeable values by branches" in {
    val conflictSet         = Set(1, 2, 3, 4, 5, 6, 7)
    val dependencyMap       = Map(1 -> Set(2), 3 -> Set(4), 4 -> Set(5))
    val rejectOptions       = Set.empty[Set[Int]]
    val initMergeableValues = Map("a" -> 0L)
    val mergeableDiffs = Map(
      // branch 1
      1 -> Map("a" -> 10L), // 10
      2 -> Map("a" -> -5L), // 5
      // branch 2
      3 -> Map("a" -> 15L),  // 20
      4 -> Map("a" -> 10L),  // 30
      5 -> Map("a" -> -20L), // 10
      // branch 3
      6 -> Map("a" -> -10L), // 0
      // branch 4
      7 -> Map("a" -> -10L) // overflow
    )
    addMergeableOverflowRejections[Int, String](
      conflictSet,
      dependencyMap,
      rejectOptions,
      initMergeableValues,
      mergeableDiffs
    ) shouldBe Set(Set(7))
  }

  "addMergeableOverflowRejections" should "reject dependent tree" in {
    val conflictSet         = Set(1, 2, 3, 4, 5, 6, 7, 12)
    val dependencyMap       = Map(1 -> Set(2), 2 -> Set(12), 3 -> Set(4, 12), 4 -> Set(5))
    val rejectOptions       = Set.empty[Set[Int]]
    val initMergeableValues = Map("a" -> 5L)
    val mergeableDiffs = Map(
      // branch 1 - branch should be rejected fully
      1 -> Map("a" -> -10L), // -5
      2 -> Map("a" -> -5L),
      // branch 2
      3 -> Map("a" -> 15L),  // 20;
      4 -> Map("a" -> 10L),  // 30
      5 -> Map("a" -> -20L), // 10
      // branch 3
      6 -> Map("a" -> -10L), // 0
      // branch 4
      7 -> Map("a" -> -10L), // overflow
      // branch 1 and 2
      // 8 depends on 3 and 1.
      // In branch 3 it does not lead to overflow.
      // But since 8 leads to overflow in branch 1 which is processed before (due to sorting of branch roots),
      // secondary attempt to fold this into result should not happen.
      12 -> Map("a" -> 10L)
    )
    addMergeableOverflowRejections[Int, String](
      conflictSet,
      dependencyMap,
      rejectOptions,
      initMergeableValues,
      mergeableDiffs
    ) shouldBe Set(Set(1, 2, 12, 7))
  }
}
