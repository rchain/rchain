package coop.rchain.casper.genesis.contracts

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.collection.EitherTest

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {
  val runtime = TestSetUtil.runtime
  val tests   = TestSetUtil.getTests("../casper/src/test/rholang/EitherTest.rho").toList

  TestSetUtil.runTestsWithDeploys(EitherTest, List(StandardDeploys.either), runtime)
  val tuplespace = StoragePrinter.prettyPrint(runtime.space.store)

  "Either rholang contract" should tests.head in {
    TestSetUtil.testPassed(tests.head, tuplespace) should be(true)
  }

  tests.tail.foreach(test => {
    it should test in {
      TestSetUtil.testPassed(test, tuplespace) should be(true)
    }
  })
}
