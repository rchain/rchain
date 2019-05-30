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
      Some("11112u2seSy3Z8NA3nj8F7ihNFMavPvtCwQbFUrJ72k8LpNs3B43oF")
    )
  }

}
