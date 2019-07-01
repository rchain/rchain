package coop.rchain.rholang.interpreter.util

import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import org.scalatest._

class RevAddressSpec extends FlatSpec with Matchers {
  "fromPublicKey" should "work correctly" in {
    val pk =
      PublicKey(
        Base16.unsafeDecode(
          "00322ba649cebf90d8bd0eeb0658ea7957bcc59ecee0676c86f4fec517c062510031122ba649cebf90d8bd0eeb0658ea7957bcc59ecee0676c86f4fec517c06251"
        )
      )
    RevAddress.fromPublicKey(pk).map(_.toBase58) should be(
      Some("1111eEt66obfqvEcKeD3ajeupzsfj6PdSQccNNaRmCoy6Dhv1wM9E")
    )
  }

}
