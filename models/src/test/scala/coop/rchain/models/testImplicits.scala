package coop.rchain.models

import com.google.protobuf.ByteString
import monix.eval.Coeval
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen, Shrink}

import scala.collection.immutable.BitSet

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

  implicit val arbSortedParMap: Arbitrary[SortedParMap] = Arbitrary(for {
    ps <- Arbitrary.arbitrary[Seq[(Par, Par)]]
  } yield SortedParMap(ps))

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
  implicit val ListParWithRandomArbitrary  = implicitly[Arbitrary[ListParWithRandom]]
  implicit val PCostArbitrary              = implicitly[Arbitrary[PCost]]
  implicit val ReceiveArbitrary            = implicitly[Arbitrary[Receive]]
  implicit val ReceiveBindArbitrary        = implicitly[Arbitrary[ReceiveBind]]
  implicit val SendArbitrary               = implicitly[Arbitrary[Send]]
  implicit val TaggedContinuationArbitrary = implicitly[Arbitrary[TaggedContinuation]]
  implicit val VarArbitrary                = implicitly[Arbitrary[Var]]
  implicit val VarRefArbitrary             = implicitly[Arbitrary[VarRef]]
  implicit val ParSetArbitrary             = implicitly[Arbitrary[ParSet]]
  implicit val ParMapArbitrary             = implicitly[Arbitrary[ParMap]]

  implicit val ParShrink                = implicitly[Shrink[Par]]
  implicit val ExprShrink               = implicitly[Shrink[Expr]]
  implicit val BindPatternShrink        = implicitly[Shrink[BindPattern]]
  implicit val BundleShrink             = implicitly[Shrink[Bundle]]
  implicit val ConnectiveShrink         = implicitly[Shrink[Connective]]
  implicit val ConnectiveBodyShrink     = implicitly[Shrink[ConnectiveBody]]
  implicit val EListShrink              = implicitly[Shrink[EList]]
  implicit val EMapShrink               = implicitly[Shrink[EMap]]
  implicit val EMatchesShrink           = implicitly[Shrink[EMatches]]
  implicit val EMethodShrink            = implicitly[Shrink[EMethod]]
  implicit val ENeqShrink               = implicitly[Shrink[ENeq]]
  implicit val ENotShrink               = implicitly[Shrink[ENot]]
  implicit val EOrShrink                = implicitly[Shrink[EOr]]
  implicit val ESetShrink               = implicitly[Shrink[ESet]]
  implicit val ETupleShrink             = implicitly[Shrink[ETuple]]
  implicit val EVarShrink               = implicitly[Shrink[EVar]]
  implicit val GPrivateShrink           = implicitly[Shrink[GPrivate]]
  implicit val KeyValuePairShrink       = implicitly[Shrink[KeyValuePair]]
  implicit val ListBindPatternsShrink   = implicitly[Shrink[ListBindPatterns]]
  implicit val MatchShrink              = implicitly[Shrink[Match]]
  implicit val MatchCaseShrink          = implicitly[Shrink[MatchCase]]
  implicit val NewShrink                = implicitly[Shrink[New]]
  implicit val ParWithRandomShrink      = implicitly[Shrink[ParWithRandom]]
  implicit val ListParWithRandomShrink  = implicitly[Shrink[ListParWithRandom]]
  implicit val PCostShrink              = implicitly[Shrink[PCost]]
  implicit val ReceiveShrink            = implicitly[Shrink[Receive]]
  implicit val ReceiveBindShrink        = implicitly[Shrink[ReceiveBind]]
  implicit val SendShrink               = implicitly[Shrink[Send]]
  implicit val TaggedContinuationShrink = implicitly[Shrink[TaggedContinuation]]
  implicit val VarShrink                = implicitly[Shrink[Var]]
  implicit val VarRefShrink             = implicitly[Shrink[VarRef]]
  implicit val ParSetShrink             = implicitly[Shrink[ParSet]]
  implicit val ParMapShrink             = implicitly[Shrink[ParMap]]

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
