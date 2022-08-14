package coop.rchain.secp256k1java

import coop.rchain.secp256k1java.NativeSecp256k1Tests._
import org.bitcoin.NativeSecp256k1
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * This class holds test cases defined for testing this library.
  */
class NativeSecp256k1Test extends AnyFlatSpec with Matchers {

  "Java bindings for Secp256k1 native library" should "work properly" in {
    //Test verify() success/fail
    testVerifyPos()
    testVerifyNeg()

    //Test secKeyVerify() success/fail
    testSecKeyVerifyPos()
    testSecKeyVerifyNeg()

    //Test computePubkey() success/fail
    testPubKeyCreatePos()
    testPubKeyCreateNeg()

    //Test sign() success/fail
    testSignPos()
    testSignNeg()

    testSignWithEntropyPos()
    testSignWithEntropyNeg()

    //Test privKeyTweakAdd() 1
    testPrivKeyTweakAdd()
    testPrivKeyTweakMul()

    testPubKeyTweakAdd()
    testPubKeyTweakMul()

    //Test randomize()
    testRandomize()

    testDecompressPubKey()

    testIsValidPubKeyPos()
    testIsValidPubKeyNeg()

    //Test ECDH
    testCreateECDHSecret()

    NativeSecp256k1.cleanup()
  }
}
