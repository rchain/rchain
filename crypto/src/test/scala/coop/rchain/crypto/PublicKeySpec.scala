package coop.rchain.crypto

import coop.rchain.shared.EqualitySpecUtils
import org.scalatest.flatspec.AnyFlatSpec

class PublicKeySpec extends AnyFlatSpec {

  "PublicKey" must "define value-based equality and hashCode" in {
    EqualitySpecUtils.checkValueBasedEquality(
      PublicKey(Array[Byte]()) ::
        PublicKey(Array[Byte](42)) ::
        PublicKey(Array[Byte](1, 2, 3)) ::
        Nil
    )
  }
}
