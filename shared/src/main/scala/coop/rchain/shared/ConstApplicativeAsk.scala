package coop.rchain.shared

import cats._, cats.data._, cats.syntax.all._, cats.mtl._
import coop.rchain.catscontrib._, Catscontrib._, ski._

class ConstApplicativeAsk[F[_]: Applicative, E](e: E) extends ApplicativeAsk[F, E] {
  val applicative: Applicative[F] = Applicative[F]
  def ask: F[E]                   = e.pure[F]
  def reader[A](f: E => A): F[A]  = applicative.map(ask)(f)
}

// TODO make availalble on catscontrib
class EitherTApplicativeAsk[F[_], E, Err](
    implicit
    ev1: ApplicativeAsk[F, E],
    ev2: Monad[F]
) extends ApplicativeAsk[EitherT[F, Err, *], E] {
  val applicative: Applicative[EitherT[F, Err, *]] = Applicative[EitherT[F, Err, *]]

  def ask: EitherT[F, Err, E] = EitherT(ev1.ask.map(Right(_)))

  def reader[A](f: E => A): EitherT[F, Err, A] = applicative.map(ask)(f)
}
