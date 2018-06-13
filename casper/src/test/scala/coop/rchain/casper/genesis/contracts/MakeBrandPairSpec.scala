package coop.rchain.casper.genesis.contracts

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.security.{MakeBrandPair, MakeBrandPairTest}

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{FlatSpec, Matchers}

class MakeBrandPairSpec extends FlatSpec with Matchers {
  val runtime = TestSetUtil.runtime("rholang-make-brand-pair-test")
  val tests   = TestSetUtil.getTests("./casper/src/test/rholang/MakeBrandPairTest.rho").toList

  TestSetUtil.runTests(MakeBrandPairTest.term, List(MakeBrandPair.term), runtime)
  val tuplespace = StoragePrinter.prettyPrint(runtime.space.store)

  "MakeBrandPair rholang contract" should tests.head in {
    TestSetUtil.testPassed(tests.head, tuplespace) should be(true)
  }

  tests.tail.map(test => {
    it should test in {
      TestSetUtil.testPassed(test, tuplespace) should be(true)
    }
  })
}
