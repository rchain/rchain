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

  //FIXME this is broken, and makes our tests blind for mishandling Option
  // The ScalaPB generators have quirks in which required objects
  // become Options. See https://github.com/scalapb/ScalaPB/issues/40 .
  // We override so that we cannot get None for these required objects.
  implicit def arbOption[T](implicit a: Arbitrary[T]): Arbitrary[Option[T]] =
    Arbitrary(for { s <- a.arbitrary } yield Some(s))

  implicit val arbByteArray: Arbitrary[ByteString] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(ba => ByteString.copyFrom(ba)))

  implicit def coeval[A: Arbitrary]: Arbitrary[Coeval[A]] =
    Arbitrary(Arbitrary.arbitrary[A].map(a => Coeval.delay(a)))

  //Par and Expr (or Par at least) need to be first here, or else the compiler dies terribly.
  implicit val ParArbitrary                = implicitly[Arbitrary[Par]]
  implicit val ExprArbitrary               = implicitly[Arbitrary[Expr]]
  implicit val BindPatternArbitrary        = implicitly[Arbitrary[BindPattern]]
  implicit val BundleArbitrary             = implicitly[Arbitrary[Bundle]]
  implicit val ConnectiveArbitrary         = implicitly[Arbitrary[Connective]]
  implicit val ConnectiveBodyArbitrary     = implicitly[Arbitrary[ConnectiveBody]]
  implicit val EListArbitrary              = implicitly[Arbitrary[EList]]
  implicit val EMapArbitrary               = implicitly[Arbitrary[EMap]]
  implicit val EMatchesArbitrary           = implicitly[Arbitrary[EMatches]]
  implicit val EMethodArbitrary            = implicitly[Arbitrary[EMethod]]
  implicit val ENeqArbitrary               = implicitly[Arbitrary[ENeq]]
  implicit val ENotArbitrary               = implicitly[Arbitrary[ENot]]
  implicit val EOrArbitrary                = implicitly[Arbitrary[EOr]]
  implicit val ESetArbitrary               = implicitly[Arbitrary[ESet]]
  implicit val ETupleArbitrary             = implicitly[Arbitrary[ETuple]]
  implicit val EVarArbitrary               = implicitly[Arbitrary[EVar]]
  implicit val GPrivateArbitrary           = implicitly[Arbitrary[GPrivate]]
  implicit val KeyValuePairArbitrary       = implicitly[Arbitrary[KeyValuePair]]
  implicit val ListBindPatternsArbitrary   = implicitly[Arbitrary[ListBindPatterns]]
  implicit val MatchArbitrary              = implicitly[Arbitrary[Match]]
  implicit val MatchCaseArbitrary          = implicitly[Arbitrary[MatchCase]]
  implicit val NewArbitrary                = implicitly[Arbitrary[New]]
  implicit val ParWithRandomArbitrary      = implicitly[Arbitrary[ParWithRandom]]
  implicit val PCostArbitrary              = implicitly[Arbitrary[PCost]]
  implicit val ReceiveArbitrary            = implicitly[Arbitrary[Receive]]
  implicit val ReceiveBindArbitrary        = implicitly[Arbitrary[ReceiveBind]]
  implicit val SendArbitrary               = implicitly[Arbitrary[Send]]
  implicit val TaggedContinuationArbitrary = implicitly[Arbitrary[TaggedContinuation]]
  implicit val VarArbitrary                = implicitly[Arbitrary[Var]]
  implicit val VarRefArbitrary             = implicitly[Arbitrary[VarRef]]
  implicit val ParSetArbitrary             = implicitly[Arbitrary[ParSet]]
  implicit val ParMapArbitrary             = implicitly[Arbitrary[ParMap]]

  implicit def alwaysEqualArbitrary[A: Arbitrary]: Arbitrary[AlwaysEqual[A]] =
    Arbitrary(Arbitrary.arbitrary[A].map(AlwaysEqual(_)))

  implicit val arbParTreeset: Arbitrary[SortedParHashSet] =
    Arbitrary(
      Arbitrary
        .arbitrary[Seq[Par]]
        .map(pars => SortedParHashSet(pars))
    )

  implicit def arbParTupleSeq: Arbitrary[Seq[(Par, Par)]] =
    Arbitrary(Gen.listOf(Gen.zip(ParArbitrary.arbitrary, ParArbitrary.arbitrary)))
}
