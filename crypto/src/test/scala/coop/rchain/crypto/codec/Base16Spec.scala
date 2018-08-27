package coop.rchain.crypto.codec

import org.scalatest._
import prop._

class Base16Spec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  property("decode after encode returns the original input") {
    forAll { (input: Array[Byte]) =>
      val encoded = Base16.encode(input)
      val decoded = Base16.decode(encoded)
      decoded should equal(input)
    }
  }
}
