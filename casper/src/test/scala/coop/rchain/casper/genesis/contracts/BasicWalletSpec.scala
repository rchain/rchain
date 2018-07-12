package coop.rchain.casper.genesis.contracts

import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.math.NonNegativeNumber
import coop.rchain.rholang.mint.MakeMint
import coop.rchain.rholang.wallet.{BasicWallet, BasicWalletTest}

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{FlatSpec, Matchers}

class BasicWalletSpec extends FlatSpec with Matchers {
  val runtime = TestSetUtil.runtime("rholang-basic-wallet-test")
  val tests   = TestSetUtil.getTests("./casper/src/test/rholang/BasicWalletTest.rho").toList

  TestSetUtil.runTests(BasicWalletTest.term,
                       List(NonNegativeNumber.term, MakeMint.term, BasicWallet.term),
                       runtime)
  val tuplespace = StoragePrinter.prettyPrint(runtime.space.store)

  "BasicWallet rholang contract" should tests.head in {
    TestSetUtil.testPassed(tests.head, tuplespace) should be(true)
  }

  tests.tail.map(test => {
    it should test in {
      TestSetUtil.testPassed(test, tuplespace) should be(true)
    }
  })
}
