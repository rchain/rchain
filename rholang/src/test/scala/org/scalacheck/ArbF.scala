package org.scalacheck

import cats.{Defer, Monad}
import magnolia._

import scala.language.experimental.macros

//FIXME add shrinking and sizing
trait ArbF[F[_[_], _], T] { def arb: F[Gen, T] }

object ArbF {

  def arbF[F[_[_], _], T](implicit ev: ArbF[F, T]): F[Gen, T] = ev.arb

  def apply[F[_[_], _], T](fgen: F[Gen, T]): ArbF[F, T] =
    new ArbF[F, T] {
      override def arb: F[Gen, T] = fgen
    }
}

import cats.implicits._

trait GenericArb[F[_[_], _]] extends GenericArbLowPriority[F] {

  implicit def defer: Defer[F[Gen, ?]]
  implicit def monad: Monad[F[Gen, ?]]

  type Typeclass[T] = ArbF[F, T]

  def combine[T](ctx: CaseClass[Typeclass, T]): ArbF[F, T] =
    ArbF(defer.defer {
      val paramFs: List[F[Gen, Any]] =
        ctx.parameters.toList.map(_.typeclass.arb.asInstanceOf[F[Gen, Any]])
      val fParams: F[Gen, List[Any]] = paramFs.sequence
      fParams.map(ctx.rawConstruct)
    })

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): ArbF[F, T] =
    ArbF(defer.defer {
      for {
        subtype <- liftF(Gen.oneOf(ctx.subtypes))
        gen     <- subtype.typeclass.arb.asInstanceOf[F[Gen, T]]
      } yield gen
    })

  def gen[T]: ArbF[F, T] = macro Magnolia.gen[T]

}

trait GenericArbLowPriority[F[_[_], _]] {

  def liftF[A](gen: Gen[A]): F[Gen, A]

  implicit def liftArbitrary[A](implicit ev: Arbitrary[A]): ArbF[F, A] =
    ArbF[F, A](liftF[A](Arbitrary.arbitrary[A]))

}
