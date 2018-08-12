package coop.rchain.casper

import cats.ApplicativeError
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{Concurrent, Timer}
import cats.implicits._
import coop.rchain.casper.protocol.ApprovedBlock

import scala.concurrent.duration.FiniteDuration

trait LastApprovedBlock[F[_]] {
  def getOptional(timeout: FiniteDuration): F[Option[ApprovedBlock]]
  def get: F[ApprovedBlock]
  def complete(a: ApprovedBlock): F[Boolean]
}

object LastApprovedBlock {
  def apply[F[_]](implicit ev: LastApprovedBlock[F]): LastApprovedBlock[F] = ev

  def of[F[_]: Concurrent: Timer]: F[LastApprovedBlock[F]] = Deferred[F, ApprovedBlock].flatMap {
    deferred =>
      Ref.of[F, Deferred[F, ApprovedBlock]](deferred).map { state =>
        new LastApprovedBlock[F] {
          override def getOptional(timeout: FiniteDuration): F[Option[ApprovedBlock]] =
            Concurrent[F].racePair(state.get.flatMap(_.get), Timer[F].sleep(timeout)).flatMap {
              case Left((lastApproved, fiberB)) =>
                fiberB.cancel.map(_ => Option(lastApproved))
              case Right((fiberA, _)) =>
                fiberA.cancel.map(_ => None)
            }
          override def get: F[ApprovedBlock] = state.get.flatMap(_.get)
          override def complete(a: ApprovedBlock): F[Boolean] =
            state.get.flatMap(
              d =>
                ApplicativeError[F, Throwable]
                  .attempt(d.complete(a))
                  .map(_.fold(_ => false, _ => true)))
        }
      }
  }

}
