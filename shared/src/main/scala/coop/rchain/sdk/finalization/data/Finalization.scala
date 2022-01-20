package coop.rchain.sdk.finalization.data

import cats.Monad
import cats.data.NonEmptyList
import coop.rchain.sdk.dag.data.{DagData, DagView}
import coop.rchain.sdk.dag.syntax._
import coop.rchain.sdk.finalization.data.Finalization.Covering
import cats.syntax.all._
import fs2.Stream

/**
  * Represents read-only view of the DAG starting from latest messages.
  */
trait Finalization[F[_], M, S] {
  implicit val ordM: Ordering[M]
  implicit val dd: DagData[M, S]
  implicit val dv: DagView[F, M, S]
  implicit val m: Monad[F]

  /**
    * Parameter for finalization to limit the range of heights after message is considered lazy
    *  and the next fringe will be calculated as subnet.
    */
  def lazyHeightLimit: Int

  /**
    * Returns latest finalized messages.
    */
  def latestFinalizedFringe: F[NonEmptyList[M]]

  /**
    * Find a finalized fringe where the message belongs, if any.
    */
  def findFinalizedFringe(msg: M): F[Option[NonEmptyList[M]]]

  /**
    * Ordered list of all finalized fringes.
    */
  def finalizedFringes: F[Seq[NonEmptyList[M]]]

  def calculateNextFringe(msg: M): F[Covering[M]] =
    Covering(msg, NonEmptyList.one(msg), NonEmptyList.one(msg)).pure[F]

}

object Finalization {
  final case class Covering[M](m: M, support: NonEmptyList[M], finalized: NonEmptyList[M])

  def apply[F[_], M, S](implicit instance: Finalization[F, M, S]): instance.type = instance
}
