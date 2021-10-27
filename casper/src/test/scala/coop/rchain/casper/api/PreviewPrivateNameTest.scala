package coop.rchain.casper.api

import cats.Id
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.catscontrib.effect.implicits._
import coop.rchain.p2p.EffectsTestInstances.LogStub
import coop.rchain.shared.Base16
import coop.rchain.shared.ByteStringOps.RichHexString
import org.scalatest.{FlatSpec, Matchers}

class PreviewPrivateNameTest extends FlatSpec with Matchers {
  implicit val logEff = new LogStub[Id]

  def previewId(pkHex: String, timestamp: Long, nth: Int = 0): String = {
    val preview = BlockAPI.previewPrivateNames[Id](
      pkHex.unsafeToByteString,
      timestamp,
      nth + 1
    )

    Base16.encode(preview.right.get(nth).toByteArray)
  }

  val myNodePk = "464f6780d71b724525be14348b59c53dc8795346dfd7576c9f01c397ee7523e6"

  "previewPrivateNames" should "work in one case" in {
    // When we deploy `new x ...` code from a javascript gRPC client,
    // we get this private name id in the log:
    // 16:41:08.995 [node-runner-15] INFO  c.r.casper.MultiParentCasperImpl - Received Deploy #1542308065454 -- new x0, x1 in {
    //   @{x1}!(...
    // [Unforgeable(0xb5630d1bfb836635126ee7f2770873937933679e38146b1ddfbfcc14d7d8a787), bundle+ {   Unforgeable(0x00) }]
    // 2018-11-15T18:54:25.454Z
    previewId(myNodePk, 1542308065454L) should equal(
      "b5630d1bfb836635126ee7f2770873937933679e38146b1ddfbfcc14d7d8a787"
    )
  }

  "previewPrivateNames" should "work for another timestamp" in {
    previewId(myNodePk, 1542315551822L) should equal(
      "d472acf9c61e276e460de567a2b709bc9b97ff6135a812abcbaa60106d2744f9"
    )
  }

  "previewPrivateNames" should "handle empty user (public key)" in {
    previewId("", 1542308065454L) should equal(
      "a249b81b82572b32e9a8adc9d708be08bc85fdf19e4aca3c316e51d30b97c993"
    )
  }

  "previewPrivateNames" should "work for more than one name" in {
    previewId(myNodePk, 1542308065454L, 1) should equal(
      "cdaba23ba96f28c7f443a84086e260b839cc33068d0f685648ba2ae08fd7f9da"
    )
  }
}
