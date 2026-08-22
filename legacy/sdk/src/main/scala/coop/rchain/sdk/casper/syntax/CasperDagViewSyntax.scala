package coop.rchain.sdk.casper.syntax

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.sdk.dag.data.DagView
import coop.rchain.sdk.dag.syntax._

/**
  * Casper specific extensions for DagView operations.
  */
trait CasperDagViewSyntax {
  implicit def sdkCasperSyntaxDagView[F[_], M, MId, S, SId](
      dagView: DagView[F, M, MId, S, SId]
  ): CasperDagViewOps[F, M, MId, S, SId] = new CasperDagViewOps(dagView)
}

final class CasperDagViewOps[F[_], M, MId, S, SId](private val dagView: DagView[F, M, MId, S, SId])
    extends AnyVal {

  /** Import of DagData implicit - necessary to enable syntax (extensions) for M and S types */
  import dagView.dd

  /**
    * Message should have block number equal to the block number of the highest justification + 1.
    */
  def checkBlockNumber(msg: M)(implicit s: Applicative[F]): F[Boolean] =
    msg.justifications.toList
      .traverse(dagView.loadMessage)
      .map(_.map(_.blockNum).max + 1)
      .map(_ != msg.blockNum)

  /**
    * Message should not have justifications past to justification of previous message from the same sender.
    *
    * TODO: Check this function! it created as an example for DagManager.
    */
  def invalidJustificationRegression(msg: M)(implicit s: Sync[F]): F[Boolean] =
    for {
      js       <- msg.justifications.toList.traverse(dagView.loadMessage)
      selfJOpt = js.find(_.sender == msg.sender)
      selfJ    <- selfJOpt.liftTo(new Exception("Message does not have self justification."))
      selfJjs  <- selfJ.justifications.toList.traverse(dagView.loadMessage)
      res = js.toIterator.map { j =>
        selfJjs.exists { selfJj =>
          // Previous justification from the same sender has seq num higher then the current one.
          selfJj.sender == j.sender && selfJj.seqNum > j.seqNum
        }
      }.nonEmpty
    } yield res

  /**
    * Message should have justification for each senders in the bonds map (both active and inactive).
    */
  def invalidJustificationFollows(msg: M, bondedSenders: Set[SId])(
      implicit s: Applicative[F]
  ): F[Boolean] =
    msg.justifications.toList
      .traverse(dagView.loadMessage)
      .map(_.map(_.sender).toSet != bondedSenders)
}
