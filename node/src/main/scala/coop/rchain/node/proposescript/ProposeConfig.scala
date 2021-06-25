package coop.rchain.node.proposescript

import cats.syntax.all._
import cats.effect.{Concurrent, Sync}
import coop.rchain.monix.Monixable
import coop.rchain.shared.Log
import fs2.Stream
import retry.{retryingOnFailures, RetryDetails, RetryPolicies, Sleep}

import scala.concurrent.duration.FiniteDuration

final case class ProposeConfig(
    leader: ValidatorProposeInfo,
    followers: List[ValidatorProposeInfo],
    checkInterval: FiniteDuration,
    proposeInterval: FiniteDuration
) {

  def logWaitProposeInterval[F[_]: Log]: F[Unit] =
    Log[F].info(s"Wait ${proposeInterval} seconds for proposing interval.")

  def proposeInLeader[F[_]: Sync: Monixable: Log]: F[String] =
    leader.deployAndProposeAsync
      .flatTap(blockHash => leader.logValidatorProposedBlock(blockHash))

  def checkLeaderBlockInFollower[F[_]: Concurrent: Monixable: Log](blockHash: String): F[Boolean] =
    Stream
      .emits(followers)
      .parEvalMapUnordered[F, Boolean](followers.length)(
        v =>
          v.isContainBlock(blockHash).flatTap { isContain =>
            v.logValidatorGotBlock(blockHash)
              .whenA(isContain) >> v.logValidatorMissedBlock(blockHash).whenA(!isContain)
          }
      )
      .forall(identity)
      .compile
      .last
      .map(_.getOrElse(false))

  def waitFollowersGotLeaderBlock[F[_]: Concurrent: Monixable: Log: Sleep](
      blockHash: String
  ): F[Boolean] = {
    val retryPolicy = RetryPolicies.constantDelay(checkInterval)
    def onFailure(failedValue: Boolean, details: RetryDetails): F[Unit] =
      Log[F].info(
        s"Retry check in ${checkInterval} because followers didn't get the block ${blockHash} from leader." +
          s"Retry ${details.retriesSoFar} times so far, cumulate cumulativeDelay:${details.cumulativeDelay}, ${failedValue}."
      )
    retryingOnFailures[Boolean](
      policy = retryPolicy,
      wasSuccessful = identity,
      onFailure = onFailure
    ) {
      checkLeaderBlockInFollower(blockHash)
    }
  }

  def proposeInFollowers[F[_]: Concurrent: Monixable: Log]: F[List[String]] =
    Stream
      .emits(followers)
      .parEvalMapUnordered[F, String](followers.length)(
        v => v.deployAndProposeAsync.flatTap(blockHash => v.logValidatorProposedBlock(blockHash))
      )
      .compile
      .toList

  def checkFollowerBlocksInLeader[F[_]: Concurrent: Monixable: Log](
      blockHashes: List[String]
  ): F[Boolean] =
    Stream
      .emits(blockHashes)
      .parEvalMapUnordered[F, Boolean](blockHashes.length)(
        b =>
          leader
            .isContainBlock(b)
            .flatTap { isContain =>
              leader
                .logValidatorGotBlock(b)
                .whenA(isContain) >> leader.logValidatorMissedBlock(b).whenA(!isContain)
            }
      )
      .forall(identity)
      .compile
      .last
      .map(_.getOrElse(false))

  def waitLeaderGotFollowerBlocks[F[_]: Concurrent: Monixable: Log: Sleep](
      blocks: List[String]
  ): F[Boolean] = {
    val retryPolicy = RetryPolicies.constantDelay(checkInterval)
    def onFailure(failedValue: Boolean, details: RetryDetails): F[Unit] =
      Log[F].info(
        s"Retry check in ${checkInterval} because leader didn't get the block from followers." +
          s"Retry ${details.retriesSoFar} times so far, cumulate cumulativeDelay:${details.cumulativeDelay}, ${failedValue}."
      )
    retryingOnFailures[Boolean](
      policy = retryPolicy,
      wasSuccessful = identity,
      onFailure = onFailure
    ) {
      checkFollowerBlocksInLeader(blocks)
    }
  }

  def proposeRound[F[_]: Concurrent: Monixable: Log: Sleep]: F[Unit] =
    for {
      _                    <- logWaitProposeInterval
      _                    <- Sleep[F].sleep(proposeInterval)
      _                    <- Log[F].info(s"Going to propose in the leader ${leader.host}.")
      proposeBlockInLeader <- proposeInLeader
      _ <- Log[F].info(
            s"Proposed successfully in the leader with new block ${proposeBlockInLeader} " +
              s"and then waiting the followers got the block."
          )
      _ <- waitFollowersGotLeaderBlock(proposeBlockInLeader)
      _ <- logWaitProposeInterval
      _ <- Sleep[F].sleep(proposeInterval)
      _ <- Log[F].info(
            s"All followers got the newly proposed block ${proposeBlockInLeader} from leader."
          )
      proposeBlocksInFollowers <- proposeInFollowers
      _ <- Log[F].info(
            s"Proposed successfully among all the followers. " +
              s"Then waiting the leader to receive all the new blocks ${proposeBlocksInFollowers}."
          )
      _ <- waitLeaderGotFollowerBlocks(proposeBlocksInFollowers)
      _ <- Log[F].info(
            "The leader successfully received all proposed blocks from followers."
          )
    } yield ()
}
