package coop.rchain.models

import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._

import scala.collection.immutable.BitSet

object testImplicits {
  val genBitSet = for { bitMask <- Arbitrary.arbitrary[Array[Long]] } yield
    BitSet.fromBitMask(bitMask)
  implicit val arbBitSet: Arbitrary[BitSet] = Arbitrary(genBitSet)

  implicit val arbPar = implicitly[Arbitrary[Par]]
}
