package coop.rchain.rholang.interpreter.matcher

import cats.implicits._
import coop.rchain.rholang.interpreter.accounting.Cost
import org.scalatest.FlatSpec

import coop.rchain.rholang.interpreter.matcher.NonDetFreeMapWithCost._
import coop.rchain.rholang.interpreter.matcher.OptionalFreeMapWithCost._

class MatcherMonadSpec extends FlatSpec {

  behavior of "MatcherMonad"

  it should "charge for each non-deterministic branch" in {
    pending

    val possibleResults = Stream((0, 1), (0, 2))
    val computation     = NonDetFreeMapWithCost.fromStream(possibleResults)
    val sum             = computation.map { case (x, y) => x + y }.charge(Cost(1))
    val (cost, _)       = sum.runWithCost(Cost(possibleResults.size)).right.get
    assert(cost.value == 0) //is 1, so just 1 is charged

    val moreVariants    = sum.flatMap(x => NonDetFreeMapWithCost.fromStream(Stream(x, 0, -x)))
    val moreComputation = moreVariants.map(x => "Do sth with " + x).charge(Cost(1))
    val (cost2, _)      = moreComputation.runWithCost(Cost(possibleResults.size * 3)).right.get
    assert(cost2.value == 0) //is 4, so only 2 is charged
  }

}
