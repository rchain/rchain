package coop.rchain.crypto

import coop.rchain.shared.EqualitySpecUtils
import org.scalatest.FlatSpec

class PublicKeySpec extends FlatSpec {

  "PublicKey" must "define value-based equality and hashCode" in {
    EqualitySpecUtils.checkValueBasedEquality(
      PublicKey(Array[Byte]()) ::
        PublicKey(Array[Byte](42)) ::
        PublicKey(Array[Byte](1, 2, 3)) ::
        Nil
    )
  }
}
