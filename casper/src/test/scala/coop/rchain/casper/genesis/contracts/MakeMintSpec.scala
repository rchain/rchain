package coop.rchain.casper.genesis.contracts

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.math.NonNegativeNumber
import coop.rchain.rholang.mint.{MakeMint, MakeMintTest}

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{FlatSpec, Matchers}

class MakeMintSpec extends FlatSpec with Matchers {
  val runtime = TestSetUtil.runtime("rholang-make-mint-test")
  val tests   = TestSetUtil.getTests("./casper/src/test/rholang/MakeMintTest.rho").toList

  TestSetUtil.runTests(MakeMintTest.term, List(NonNegativeNumber.term, MakeMint.term), runtime)
  val tuplespace = StoragePrinter.prettyPrint(runtime.space.store)

  "MakeMint rholang contract" should tests.head in {
    TestSetUtil.testPassed(tests.head, tuplespace) should be(true)
  }

  tests.tail.map(test => {
    it should test in {
      TestSetUtil.testPassed(test, tuplespace) should be(true)
    }
  })
}
