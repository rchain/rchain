package coop.rchain.crypto.signatures

import org.scalatest.flatspec.AnyFlatSpec

class Secp256k1Spec extends AnyFlatSpec {

  "Secp256k1" should "generate valid key pair" in {
    assert((1 to 1000).forall { _ =>
      Secp256k1.newKeyPair
      true
    })
  }
}
