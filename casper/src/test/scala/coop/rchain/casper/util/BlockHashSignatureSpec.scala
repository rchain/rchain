package coop.rchain.casper.util

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, Justification}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.models.blockImplicits._

class BlockHashSignatureSpec extends FlatSpec with Matchers with PropertyChecks {

  implicit val aBlock     = arbBlockMessage
  implicit val aValidator = arbitraryValidator

  "block hash" should "be created from all fields except signature" in {
    forAll { (block: BlockMessage, bytes: ByteString) =>
      val hash = ProtoUtil.hashBlock(block)
      val blockModifiedJustifications = block.copy(
        justifications = List(Justification(bytes, bytes))
      )
      val hashModified = ProtoUtil.hashBlock(blockModifiedJustifications)

      hashModified shouldBe hash
    }
  }

}
