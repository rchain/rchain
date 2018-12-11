package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.util.ProtoUtil.compiledSourceDeploy
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.collection.Either
import coop.rchain.rholang.math.NonNegativeNumber
import coop.rchain.rholang.mint.MakeMint
import coop.rchain.rholang.proofofstake.{MakePoS, MakePoSTest}

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{FlatSpec, Matchers}

class MakePoSSpec extends FlatSpec with Matchers {
  val runtime = TestSetUtil.runtime
  val tests   = TestSetUtil.getTests("../casper/src/test/rholang/MakePoSTest.rho").toList

  val deploys = List(
    StandardDeploys.nonNegativeNumber,
    StandardDeploys.makeMint,
    StandardDeploys.either,
    StandardDeploys.makePoS
  )
  TestSetUtil.runTestsWithDeploys(MakePoSTest, deploys, runtime)
  val tuplespace = StoragePrinter.prettyPrint(runtime.space.store)

  "MakePoS rholang contract" should tests.head in {
    TestSetUtil.testPassed(tests.head, tuplespace) should be(true)
  }

  tests.tail.map(test => {
    it should test in {
      TestSetUtil.testPassed(test, tuplespace) should be(true)
    }
  })
}
