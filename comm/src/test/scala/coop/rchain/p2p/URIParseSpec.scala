package coop.rchain.p2p

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import coop.rchain.comm._

class URIParseSpec extends AnyFlatSpec with Matchers {
  def badAddressError(s: String): Either[ParseError, PeerNode] =
    Left(ParseError(s"bad address: $s"))

  "A well formed rnode URI" should "parse into a PeerNode" in {
    val uri = "rnode://abcdef@localhost?protocol=12345&discovery=12346"
    PeerNode.fromAddress(uri) should be(
      Right(
        PeerNode(
          NodeIdentifier(Seq(0xAB.toByte, 0xCD.toByte, 0xEF.toByte)),
          Endpoint("localhost", 12345, 12346)
        )
      )
    )
  }

  "A non-rnode URI" should "parse as an error" in {
    val uri = "http://foo.bar.baz/quux"
    PeerNode.fromAddress(uri) should be(badAddressError(uri))
  }

  "A URI without protocol" should "parse as an error" in {
    val uri = "abcde@localhost?protocol=12345&discovery=12346"
    PeerNode.fromAddress(uri) should be(badAddressError(uri))
  }

  "An rnode URI with non-integral port" should "parse as an error" in {
    val uri = "rnode://abcde@localhost:smtp"
    PeerNode.fromAddress(uri) should be(badAddressError(uri))
  }

  "An rnode URI without a key" should "parse as an error" in {
    val uri = "rnode://localhost?protocol=12345&discovery=12346"
    PeerNode.fromAddress(uri) should be(badAddressError(uri))
  }
}
