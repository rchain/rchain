package coop.rchain.rholang.interpreter.util.codec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop._

class Base58Spec extends AnyFlatSpec with TableDrivenPropertyChecks with Matchers {
  "encode empty array" should "be empty" in {
    val input = Array[Byte]()
    Base58.encode(input) should equal("")
  }

  val examples: TableFor2[BigInt, String] =
    Table(
      ("plain", "encoded"),
      (0, "1"),
      (58, "21"),
      (58 * 58, "211"),
      (58 * 58 * 58, "2111"),
      (
        BigInt(
          "0D944FC57C061C7417A29357510FB866E8C5E4DE7DAA101437C133DA0AEADEE0BA7C50783DC62A641155A5",
          16
        ),
        "2123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
      )
    )

  "encoding" should "work as expected" in {
    forAll(examples) { (plain: BigInt, encoded: String) =>
      val plainBytes = plain.toByteArray
      Base58.encode(plainBytes) should equal(encoded)
    }
  }

  "decoding" should "work as expected" in {
    forAll(examples) { (plain: BigInt, encoded: String) =>
      val plainBytes = plain.toByteArray
      Base58.decode(encoded).get should equal(plainBytes)
    }
  }

  "decoding" should "fail if input contains invalid chars" in {
    Base58.decode("0").isEmpty should be(true)
  }
}
