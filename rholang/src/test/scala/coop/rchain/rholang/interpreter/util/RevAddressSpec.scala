package coop.rchain.rholang.interpreter.util

import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import org.scalatest._

class RevAddressSpec extends FlatSpec with Matchers {
  "fromPublicKey" should "work correctly" in {
    val pk = PublicKey(Base16.decode("00322ba649cebf90d8bd0eeb0658ea7957bcc59ecee0676c86f4fec517c06251"))
    RevAddress.fromPublicKey(pk) should be(
      Some("1111K9MczqzZrNkUNmNGrNFyz7F7LiCUgaCHXd28g2k5PxiaNuCAi")
    )
  }

}
