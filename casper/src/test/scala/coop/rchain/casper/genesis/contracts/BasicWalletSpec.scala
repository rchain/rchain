package coop.rchain.casper.genesis.contracts

import java.io.StringReader

import coop.rchain.casper.util.ProtoUtil.compiledSourceDeploy
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Ed25519
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.Interpreter
import coop.rchain.rholang.interpreter.storage.StoragePrinter
import coop.rchain.rholang.math.NonNegativeNumber
import coop.rchain.rholang.mint.MakeMint
import coop.rchain.rholang.wallet.{BasicWallet, BasicWalletTest}
import coop.rchain.rspace.Serialize
import monix.execution.Scheduler.Implicits.global
import org.abstractj.kalium.NaCl

import org.scalatest.{FlatSpec, Matchers}

class BasicWalletSpec extends FlatSpec with Matchers {
  val runtime = TestSetUtil.runtime
  val tests   = TestSetUtil.getTests("../casper/src/test/rholang/BasicWalletTest.rho").toList

  val deploys = List(
    StandardDeploys.nonNegativeNumber,
    StandardDeploys.makeMint,
    StandardDeploys.basicWallet
  )
  TestSetUtil.runTestsWithDeploys(BasicWalletTest, deploys, runtime)
  val tuplespace = StoragePrinter.prettyPrint(runtime.space.store)

  "Kalium" should "work" in {
    val sodium = NaCl.sodium()
    println(sodium.sodium_version_string())
  }

  "BasicWallet rholang contract" should tests.head in {
    TestSetUtil.testPassed(tests.head, tuplespace) should be(true)
  }

  tests.tail.map(test => {
    it should test in {
      TestSetUtil.testPassed(test, tuplespace) should be(true)
    }
  })
}

/**
  * A tool for generating withdrawal signatures for the BasicWalletTest.rho
  */
object Signer {

  def main(args: Array[String]): Unit = {
    val sigKey = "1388803416a5869f3d4682fb3fae738278287b80d1a5a52ddf89be8eb9dac59d"
    println(signWithdrawal(0, 60, sigKey))
    println(signWithdrawal(1, 10, sigKey))
  }

  private def signWithdrawal(nonce: Int, amount: Int, sigKey: String): String = {
    def parse(rho: String): Par =
      Interpreter
        .buildNormalizedTerm(rho)
        .value

    def bytes(par: Par): Array[Byte] = {
      import coop.rchain.models.serialization.implicits._
      Serialize[Par].encode(par).toArray
    }

    val value       = s"""[$nonce, $amount, "myWithdraw"]"""
    val data        = parse(value)
    val dataBytes   = bytes(data)
    val sigKeyBytes = Base16.decode(sigKey)
    val sigBytes    = Ed25519.sign(Blake2b256.hash(dataBytes), sigKeyBytes)
    val sig         = Base16.encode(sigBytes)
    sig
  }
}
