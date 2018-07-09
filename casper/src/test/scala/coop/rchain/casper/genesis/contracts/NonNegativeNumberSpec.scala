package coop.rchain.casper.genesis.contracts

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.math.{NonNegativeNumber, NonNegativeNumberTest}

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{FlatSpec, Matchers}

class NonNegativeNumberSpec extends FlatSpec with Matchers {
  val runtime = TestSetUtil.runtime("rholang-non-negative-number-test")
  val tests   = TestSetUtil.getTests("./casper/src/test/rholang/NonNegativeNumberTest.rho").toList

  TestSetUtil.runTests(NonNegativeNumberTest.term, List(NonNegativeNumber.term), runtime)
  val tuplespace = StoragePrinter.prettyPrint(runtime.space.store)

  "NonNegativeNumber rholang contract" should tests.head in {
    TestSetUtil.testPassed(tests.head, tuplespace) should be(true)
  }

  tests.tail.map(test => {
    it should test in {
      TestSetUtil.testPassed(test, tuplespace) should be(true)
    }
  })
}
