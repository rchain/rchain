package coop.rchain.casper.genesis.contracts

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.math.{NonNegativeNumber, NonNegativeNumberTest}

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{FlatSpec, Matchers}

class NonNegativeNumberSpec extends FlatSpec with Matchers {
  val runtime = TestSetUtil.runtime
  val tests   = TestSetUtil.getTests("../casper/src/test/rholang/NonNegativeNumberTest.rho").toList

  TestSetUtil.runTestsWithDeploys(
    NonNegativeNumberTest,
    List(StandardDeploys.nonNegativeNumber),
    runtime
  )
  val tuplespace = StoragePrinter.prettyPrint(runtime.space.store)

  "NonNegativeNumber rholang contract" should tests.head in {
    TestSetUtil.testPassed(tests.head, tuplespace) should be(true)
  }

  tests.tail.foreach(test => {
    it should test in {
      TestSetUtil.testPassed(test, tuplespace) should be(true)
    }
  })
}
