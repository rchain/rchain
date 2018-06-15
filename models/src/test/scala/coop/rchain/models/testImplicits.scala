package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.models.Expr.ExprInstance
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.Gen.{const, frequency, resize, sized}
import coop.rchain.models.rholang.sort.ordering._

import scala.collection.immutable.BitSet
import monix.eval.Coeval

object testImplicits {
  val genBitSet = for { bitMask <- Arbitrary.arbitrary[Array[Long]] } yield
    BitSet.fromBitMask(bitMask)
  implicit val arbBitSet: Arbitrary[BitSet] = Arbitrary(genBitSet)

  // The ScalaPB generators have quirks in which required objects
  // become Options. See https://github.com/scalapb/ScalaPB/issues/40 .
  // We override so that we cannot get None for these required objects.
  implicit def arbOption[T](implicit a: Arbitrary[T]): Arbitrary[Option[T]] =
    Arbitrary(for { s <- a.arbitrary } yield Some(s))

  implicit val arbByteArray: Arbitrary[ByteString] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(ba => ByteString.copyFrom(ba)))

  implicit def coeval[A: Arbitrary]: Arbitrary[Coeval[A]] =
    Arbitrary(Arbitrary.arbitrary[A].map(a => Coeval.delay(a)))

  implicit val arbPar = implicitly[Arbitrary[Par]]

  implicit val arbParTreeset: Arbitrary[SortedParHashSet] =
    Arbitrary(
      Arbitrary
        .arbitrary[Seq[Par]]
        .map(pars => SortedParHashSet(pars)))

  implicit def arbParTupleSeq: Arbitrary[Seq[(Par, Par)]] =
    Arbitrary(Gen.listOf(Gen.zip(arbPar.arbitrary, arbPar.arbitrary)))
}
