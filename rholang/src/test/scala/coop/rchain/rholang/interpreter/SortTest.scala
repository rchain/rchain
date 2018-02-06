package coop.rchain.rholang.interpreter

import org.scalatest._

class ScoredTermSpec extends FlatSpec with Matchers {
  "ScoredTerm" should "Sort so that shorter nodes come first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Leaves(1,2,2,3)), ScoredTerm("bar", Leaves(1,2,2)))
    val sortedTerms = Seq(ScoredTerm("bar", Leaves(1,2,2)), ScoredTerm("foo", Leaves(1,2,2,3)))
    unsortedTerms.sorted should be (sortedTerms)
  }
  "ScoredTerm" should "Sort so that smaller leafs stay first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Leaf(1)), ScoredTerm("bar", Leaf(2)))
    val sortedTerms = Seq(ScoredTerm("foo", Leaf(1)), ScoredTerm("bar", Leaf(2)))
    unsortedTerms.sorted should be (sortedTerms)
  }
  "ScoredTerm" should "Sort so that smaller leafs are put first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Leaf(2)), ScoredTerm("bar", Leaf(1)))
    val sortedTerms = Seq(ScoredTerm("bar", Leaf(1)), ScoredTerm("foo", Leaf(2)))
    unsortedTerms.sorted should be (sortedTerms)
  }
  "ScoredTerm" should "Sort so that smaller nodes are put first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Node(Seq(Leaves(1,2), Leaves(2,2)))),
      ScoredTerm("bar", Node(Seq(Leaves(1,1), Leaves(2,2)))))
    val sortedTerms = Seq(ScoredTerm("bar", Node(Seq(Leaves(1,1), Leaves(2,2)))),
      ScoredTerm("foo", Node(Seq(Leaves(1,2), Leaves(2,2)))))
    unsortedTerms.sorted should be (sortedTerms)
  }
}