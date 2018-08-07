package coop.rchain.shared

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._
import cats.mtl._

class ConstApplicativeAsk[F[_]: Applicative, E](e: E) extends ApplicativeAsk[F, E] {
  val applicative: Applicative[F] = Applicative[F]
  def ask: F[E]                   = e.pure[F]
  def reader[A](f: E => A): F[A]  = applicative.map(ask)(f)
}
