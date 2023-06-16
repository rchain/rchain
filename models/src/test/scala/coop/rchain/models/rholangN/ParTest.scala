package coop.rchain.models.rholangN

import coop.rchain.models.rholangN
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scodec.bits.ByteVector

class ParSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {

  behavior of "Par"

  it should "test Pars" in {
    val left            = GNilN()
    val tmp: ByteVector = left.toBytes
    val right           = ParN.fromBytes(tmp)
    left should be(right)
  }
}
