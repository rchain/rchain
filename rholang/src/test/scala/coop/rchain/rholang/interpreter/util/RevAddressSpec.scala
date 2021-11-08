package coop.rchain.rholang.interpreter.util

import coop.rchain.crypto.PublicKey
import coop.rchain.shared.Base16
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

  "fromEthAddress" should "work correctly without a prefix" in {
    val ethAddress = "06a441c277bf454c5d159b0e5bdafca69b296733"
    RevAddress.fromEthAddress(ethAddress).map(_.toBase58) should be(
      Some("1111Gzo7ywxbcXVumSS9Lzd8JBAqnF1zniNszvMLHQ2APa3dzs2rG")
    )
  }

  "fromEthAddress" should "work correctly with a prefix" in {
    val ethAddress = "0x06a441c277bf454c5d159b0e5bdafca69b296733"
    RevAddress.fromEthAddress(ethAddress).map(_.toBase58) should be(
      Some("1111Gzo7ywxbcXVumSS9Lzd8JBAqnF1zniNszvMLHQ2APa3dzs2rG")
    )
  }

  "fromEthAddress" should "fail when wrong prefix" in {
    val ethAddress = "1x06a441c277bf454c5d159b0e5bdafca69b296733"
    RevAddress.fromEthAddress(ethAddress).map(_.toBase58) should be(None)
  }

  "fromEthAddress" should "fail when wrong length" in {
    val ethAddress = "0x06"
    RevAddress.fromEthAddress(ethAddress).map(_.toBase58) should be(None)
  }
}
