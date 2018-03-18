package coop.rchain.models

import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.Gen.{const, frequency, resize, sized}

import scala.collection.immutable.BitSet

object testImplicits {
  val genBitSet = for { bitMask <- Arbitrary.arbitrary[Array[Long]] } yield
    BitSet.fromBitMask(bitMask)
  implicit val arbBitSet: Arbitrary[BitSet] = Arbitrary(genBitSet)

  // The ScalaPB generators have quirks in which required objects
  // become Options. See https://github.com/scalapb/ScalaPB/issues/40 .
  // We override so that we cannot get None for these required objects.
  implicit def arbOption[T](implicit a: Arbitrary[T]): Arbitrary[Option[T]] =
    Arbitrary(for { s <- a.arbitrary } yield Some(s))

  implicit val arbPar = implicitly[Arbitrary[Par]]
}
