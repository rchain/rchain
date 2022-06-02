package coop.rchain.casper.genesis.contracts

import coop.rchain.shared.Base16
import org.scalatest.flatspec.AnyFlatSpec

class StandardDeploysSpec extends AnyFlatSpec {
  it should "print public keys used for signing standard (blessed) contracts" in {
    println(s"Public keys used to sign standard (blessed) contracts")
    println(s"=====================================================")

    StandardDeploys.systemPublicKeys.zipWithIndex.foreach {
      case (pubKey, idx) =>
        println(s"${idx + 1}. ${Base16.encode(pubKey.bytes)}")
    }
  }
}
