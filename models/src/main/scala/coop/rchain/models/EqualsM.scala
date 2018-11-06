package coop.rchain.models

import cats.effect.Sync
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.catscontrib.Catscontrib._
import coop.rchain.crypto.hash.Blake2b512Random
import monix.eval.Coeval

import scala.Function.tupled
import scala.collection.immutable.BitSet

/**
  * A typeclass for computing Scala's default `equals` for case classes in a stacksafe manner.
  *
  * An instance of this class must be provided for every case where `equals` is overridden.
  * The generative tests should catch that, provided the offending case class is used transitively.
  *
  * @tparam A
  */
trait EqualM[A] {

  def equal[F[_]: Sync](self: A, other: A): F[Boolean]

  def equals[F[_]: Sync](a: A, b: Any): F[Boolean] =
    if (a.getClass.isInstance(b)) {
      equal[F](a, a.getClass.cast(b))
    } else false.pure[F]

}

object EqualM extends EqualMDerivation {

  def apply[A](implicit ev: EqualM[A]): EqualM[A] = ev

  def by[A, B: EqualM](f: A => B): EqualM[A] = new EqualM[A] {

    override def equal[F[_]: Sync](self: A, other: A): F[Boolean] =
      EqualM[B].equal(f(self), f(other))

  }

  /**
    * EqualM instance delegating to Any#equa(). Hence the type bound.
    * This MUST NOT be used for types having a recursive structure
    * nor for types whose EqualCode() delegates to others.
    * Otherwise the stacksafety provided by the EqualM typeclass is broken.
    *
    * @tparam A
    * @return
    */
  def opaqueEqual[A]: EqualM[A] = new EqualM[A] {
    override def equal[F[_]: Sync](self: A, other: A): F[Boolean] = Sync[F].pure(self == other)
  }

  implicit val IntEqual: EqualM[Int]                           = opaqueEqual
  implicit val FloatEqual: EqualM[Float]                       = opaqueEqual
  implicit val LongEqual: EqualM[Long]                         = opaqueEqual
  implicit val DoubleEqual: EqualM[Double]                     = opaqueEqual
  implicit val StringEqual: EqualM[String]                     = opaqueEqual
  implicit val BooleanEqual: EqualM[Boolean]                   = opaqueEqual
  implicit val BitSetEqual: EqualM[BitSet]                     = opaqueEqual
  implicit val ByteStringEqual: EqualM[ByteString]             = opaqueEqual
  implicit val Blake2b512RandomEqual: EqualM[Blake2b512Random] = opaqueEqual
  implicit def alwaysEqualEqual[A]: EqualM[AlwaysEqual[A]]     = opaqueEqual

  implicit def seqEqual[A: EqualM]: EqualM[Seq[A]] = new EqualM[Seq[A]] {

    override def equal[F[_]: Sync](self: Seq[A], other: Seq[A]): F[Boolean] = {
      val pairs = self.toStream.zip(other)
      Sync[F].delay(self.length == other.length) <&&>
        pairs.forallM(tupled(EqualM[A].equal[F]))
    }

  }

  implicit def coevalEqual[A: EqualM]: EqualM[Coeval[A]] = by(_.value)

  implicit val ParEqual: EqualM[Par] = gen[Par]
  implicit val ExprEqual             = gen[Expr]
  implicit val VarEqual              = gen[Var]
  implicit val SendEqual             = gen[Send]
  implicit val ReceiveEqual          = gen[Receive]
  implicit val ReceiveBindEqual      = gen[ReceiveBind]
  implicit val NewEqual              = gen[New]
  implicit val MatchEqual            = gen[Match]

  implicit val ConnectiveEqual = gen[Connective]

  implicit val ESetEqual = gen[ESet]
  implicit val EMapEqual = gen[EMap]

  implicit val SortedParHashSetEqual: EqualM[SortedParHashSet] = by(_.sortedPars)
  implicit val SortedParMapEqual: EqualM[SortedParMap]         = by(_.sortedList)

  implicit val ParSetEqual: EqualM[ParSet] = by(x => (x.ps, x.remainder, x.connectiveUsed))
  implicit val ParMapEqual: EqualM[ParMap] = by(x => (x.ps, x.remainder, x.connectiveUsed))

  implicit val BlockInfoHash                  = gen[BlockInfo]
  implicit val BlockInfoWithoutTuplespaceHash = gen[BlockInfoWithoutTuplespace]
  implicit val BlockQueryResponseHash         = gen[BlockQueryResponse]
  implicit val ContinuationsWithBlockInfoHash = gen[ContinuationsWithBlockInfo]
  implicit val DataWithBlockInfoHash          = gen[DataWithBlockInfo]
  implicit val WaitingContinuationInfoHash    = gen[WaitingContinuationInfo]

  implicit val ApprovedBlockHash             = gen[ApprovedBlock]
  implicit val ApprovedBlockCandidateHash    = gen[ApprovedBlockCandidate]
  implicit val BlockApprovalHash             = gen[BlockApproval]
  implicit val BlockMessageHash              = gen[BlockMessage]
  implicit val BodyHash                      = gen[Body]
  implicit val BondHash                      = gen[Bond]
  implicit val DeployHash                    = gen[Deploy]
  implicit val DeployDataHash                = gen[DeployData]
  implicit val FindDeployInBlockQueryHash    = gen[FindDeployInBlockQuery]
  implicit val HeaderHash                    = gen[Header]
  implicit val MaybeBlockMessageHash         = gen[MaybeBlockMessage]
  implicit val ProcessedDeployHash           = gen[ProcessedDeploy]
  implicit val RChainStateHash               = gen[RChainState]
  implicit val UnapprovedBlockHash           = gen[UnapprovedBlock]
  implicit val ListParWithRandomAndPhlosHash = gen[ListParWithRandomAndPhlos]

  implicit val PCostHash              = gen[PCost]
  implicit val TaggedContinuationHash = gen[TaggedContinuation]

  implicit val PrivateNamePreviewQueryHash = gen[PrivateNamePreviewQuery]
}

trait EqualMDerivation {
  import magnolia._

  type Typeclass[T] = EqualM[T]

  def combine[T](ctx: CaseClass[EqualM, T]): EqualM[T] = new EqualM[T] {

    def equal[F[_]: Sync](self: T, other: T): F[Boolean] = Sync[F].defer {
      ctx.parameters.toStream.forallM { p =>
        p.typeclass.equal(p.dereference(self), p.dereference(other))
      }
    }

  }

  def dispatch[T](ctx: SealedTrait[EqualM, T]): EqualM[T] = new EqualM[T] {

    def equal[F[_]: Sync](self: T, other: T): F[Boolean] = Sync[F].defer {
      ctx.dispatch(self) { sub =>
        if (sub.cast.isDefinedAt(other)) {
          sub.typeclass.equal(sub.cast(self), sub.cast(other))
        } else {
          Sync[F].pure(false)
        }
      }
    }

  }

  implicit def gen[T]: EqualM[T] = macro Magnolia.gen[T]
}
