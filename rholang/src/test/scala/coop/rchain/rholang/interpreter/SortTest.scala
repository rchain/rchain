package coop.rchain.rholang.interpreter

import org.scalatest._

class ScoredTermSpec extends FlatSpec with Matchers {
  "ScoredTerm" should "Sort so that shorter terms come first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Seq(1,2,2,3)), ScoredTerm("bar", Seq(1,2,2)))
    val sortedTerms = Seq(ScoredTerm("bar", Seq(1,2,2)), ScoredTerm("foo", Seq(1,2,2,3)))
    unsortedTerms.sorted should be (sortedTerms)
  }
  "ScoredTerm" should "Sort so that smaller scores stay first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Seq(1)), ScoredTerm("bar", Seq(2)))
    val sortedTerms = Seq(ScoredTerm("foo", Seq(1)), ScoredTerm("bar", Seq(2)))
    unsortedTerms.sorted should be (sortedTerms)
  }
  "ScoredTerm" should "Sort so that smaller scores are put first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Seq(2)), ScoredTerm("bar", Seq(1)))
    val sortedTerms = Seq(ScoredTerm("bar", Seq(1)), ScoredTerm("foo", Seq(2)))
    unsortedTerms.sorted should be (sortedTerms)
  }
  "ScoredTerm" should "Sort so that empty scores are put first" in {
    val unsortedTerms = Seq(ScoredTerm("foo", Seq(2)), ScoredTerm("bar", Nil))
    val sortedTerms = Seq(ScoredTerm("bar", Nil), ScoredTerm("foo", Seq(2)))
    unsortedTerms.sorted should be (sortedTerms)
  }
}