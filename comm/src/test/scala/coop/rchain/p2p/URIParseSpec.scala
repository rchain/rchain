package coop.rchain.p2p

import org.scalatest._
import coop.rchain.comm._

class URIParseSpec extends FlatSpec with Matchers {
  def badAddressError(s: String): Either[ParseError, PeerNode] =
    Left(ParseError(s"bad address: $s"))

  "A well formed rnode URI" should "parse into a PeerNode" in {
    val uri = "rnode://abcdef@localhost:12345"
    PeerNode.parse(uri) should be(
      Right(PeerNode(NodeIdentifier(Seq(0xAB.toByte, 0xCD.toByte, 0xEF.toByte)),
                     Endpoint("localhost", 12345, 12345))))
  }

  "A non-rnode URI" should "parse as an error" in {
    val uri = "http://foo.bar.baz/quux"
    PeerNode.parse(uri) should be(badAddressError(uri))
  }

  "A URI without protocol" should "parse as an error" in {
    val uri = "abcde@localhost:12345"
    PeerNode.parse(uri) should be(badAddressError(uri))
  }

  "An rnode URI with non-integral port" should "parse as an error" in {
    val uri = "rnode://abcde@localhost:smtp"
    PeerNode.parse(uri) should be(badAddressError(uri))
  }

  "An rnode URI without a key" should "parse as an error" in {
    val uri = "rnode://localhost:1234"
    PeerNode.parse(uri) should be(badAddressError(uri))
  }
}
