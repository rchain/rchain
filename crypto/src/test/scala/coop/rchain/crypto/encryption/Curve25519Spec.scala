package coop.rchain.crypto.encryption

import org.scalatest._
import org.scalatestplus.scalacheck._

class Curve25519Spec extends PropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  property("encrypt and decrypt should give original message") {
    forAll((message: Array[Byte]) => {
      // given
      val nonce     = Curve25519.newNonce
      val aliceKeys = Curve25519.newKeyPair
      val bobKeys   = Curve25519.newKeyPair
      // when
      val cipher = Curve25519.encrypt(aliceKeys._1, bobKeys._2, nonce, message)
      val plain  = Curve25519.decrypt(bobKeys._1, aliceKeys._2, nonce, cipher)
      // then
      message should equal(plain)
    })
  }
}
