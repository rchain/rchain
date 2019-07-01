package coop.rchain.crypto

import coop.rchain.shared.EqualitySpecUtils
import org.scalatest.FlatSpec

class PrivateKeySpec extends FlatSpec {

  "PrivateKey" must "define value-based equality and hashCode" in {
    EqualitySpecUtils.checkValueBasedEquality(
      PrivateKey(Array[Byte]()) ::
        PrivateKey(Array[Byte](42)) ::
        PrivateKey(Array[Byte](1, 2, 3)) ::
        Nil
    )
  }
}
