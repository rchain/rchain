package coop.rchain.casper.genesis.contracts

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.collection.{Either, EitherTest}

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {
  val runtime = TestSetUtil.runtime("rholang-either-test")
  val tests   = TestSetUtil.getTests("./casper/src/test/rholang/EitherTest.rho").toList

  TestSetUtil.runTests(EitherTest.term, List(Either.term), runtime)
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
