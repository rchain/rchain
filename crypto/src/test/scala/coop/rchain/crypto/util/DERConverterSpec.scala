package coop.rchain.crypto.util

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.scalacheck.Prop.propBoolean

class DERConverterSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {
  implicit val arbBytes: Arbitrary[Array[Byte]] = arbContainer[Array, Byte]

  property("DER converter should work with any non-empty byte array") {
    forAll { (bytes: Array[Byte]) =>
      !bytes.isEmpty ==> !CertificateHelper.encodeSignatureRStoDER(bytes).right.get.isEmpty

      !bytes.isEmpty ==> !CertificateHelper.decodeSignatureDERtoRS(bytes).right.get.isEmpty
    }
  }

}
