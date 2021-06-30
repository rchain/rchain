package coop.rchain.casper.finality

import cats.Applicative
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.safety.CliqueOracle
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream

/**
  * Block can be recorded as last finalized block (LFB) if Safety oracle outputs fault tolerance (FT)
  * for this block greater then some predefined threshold. This is defined by [[CliqueOracle.computeOutput]]
  * function, which requires some target block as input arg.
  *
  * Therefore: Finalizer has a scope of search, defined by tips and previous LFB - each of this blocks can be next LFB.
  *
  * We know that LFB advancement is not necessary continuous, next LFB might not be direct child of current one.
  *
  * Therefore: we cannot start from current LFB children and traverse DAG from the bottom to the top, calculating FT
  * for each block. Also its computationally ineffective.
  *
  * But we know that scope of search for potential next LFB is constrained. Block A can be finalized only
  * if it has more then half of total stake in bonds map of A translated from tips throughout main parent chain.
  * IMPORTANT: only main parent relation gives weight to potentially finalized block.
  *
  * Therefore: Finalizer should seek for next LFB going through 2 steps:
  *   1. Find messages in scope of search that have more then half of the stake translated through main parent chain
  *     from tips down to the message.
  *   2. Execute [[CliqueOracle.computeOutput]] on these targets.
  *   3. First message passing FT threshold becomes the next LFB.
  */
object Finalizer {

  private type WeightMap = Map[Validator, Long]

  /** Message that is agreed on + weight of this agreement. */
  final case class MessageAgreement(
      message: BlockMetadata,
      messageWeightMap: WeightMap,
      stakeAgreed: WeightMap
  )

  /** weight map as per message, look inside [[CliqueOracle.getCorrespondingWeightMap]] description for more info */
  private def messageWeightMapF[F[_]: Sync](
      message: BlockMetadata,
      dag: BlockDagRepresentation[F]
  ): F[WeightMap] = CliqueOracle.getCorrespondingWeightMap(message.blockHash, dag)

  /** If more then half of total stake agree on message - it is considered to be safe from orphaning. */
  def cannotBeOrphaned[F[_]: Applicative](
      messageWeightMap: WeightMap,
      agreeingWeightMap: WeightMap
  ): Boolean = {
    require(
      !agreeingWeightMap.exists { case (_, stake) => stake <= 0 },
      message = "Agreeing map contains not bonded validators"
    )
    val activeStakeTotal    = messageWeightMap.values.sum
    val activeStakeAgreeing = agreeingWeightMap.values.sum
    // in theory if each stake is high enough, e.g. Long.MaxValue, sum of them might result in negative value
    require(activeStakeTotal > 0, message = "Long overflow when computing total stake")
    require(activeStakeAgreeing > 0, message = "Long overflow when computing total stake")
    activeStakeAgreeing > activeStakeTotal.toFloat / 2
  }

  /**
    * Create an agreement given validator that agrees on a message and weight map of a message.
    * If validator is not present in message bonds map or its stake is zero, None is returned
    */
  private def recordAgreement(
      messageWeightMap: WeightMap,
      agreeingValidator: Validator
  ): Option[(Validator, Long)] = {
    // if validator is not bonded according to message weight map - there is no agreement translated.
    val stakeAgreed = messageWeightMap.getOrElse(agreeingValidator, 0L)
    if (stakeAgreed > 0) (agreeingValidator -> stakeAgreed).some else None
  }

  /**
    * Find the highest finalized message.
    * Scope of the search is constrained by the lowest height (height of current last finalized message).
    */
  def run[F[_]: Concurrent: Metrics: Log: Span](
      dag: BlockDagRepresentation[F],
      faultToleranceThreshold: Float,
      currLFBHeight: Long,
      newLfbEffect: BlockHash => F[Unit],
      finalisationEffect: Set[BlockHash] => F[Unit]
  ): F[Option[BlockHash]] = {

    /**
      * Stream of agreements passed down from all latest messages to main parents.
      * Starts with agreements of latest message on themselves.
      *
      * The goal here is to create stream of agreements breadth first, so on each step agreements by all
      * validator are recorded, and only after that next level of main parents is visited.
      */
    val mkAgreementsStream: F[Stream[F, MessageAgreement]] =
      dag.latestMessages.map { lms =>
        // sort latest messages by agreeing validator to ensure random ordering does not change output
        val sortedLatestMessages = lms.toList.sortBy { case (v, _) => v }
        Stream
          .unfoldLoopEval(sortedLatestMessages) { layer =>
            // output current visits
            val out = layer
            // proceed to main parents
            val nextF = layer
              .traverse {
                case (v, message) =>
                  message.parents.headOption.traverse(dag.lookupUnsafe).map { messageMainParent =>
                    (v, messageMainParent)
                  }
              }
              // filter out empty results when no main parent and those out of scope
              .map(_.collect { case (v, Some(meta)) if meta.blockNum > currLFBHeight => (v, meta) })

            nextF.map(next => (out, next.nonEmpty.guard[Option].as(next)))
          }
          .evalMap(_.traverse {
            // map visits to message agreements: validator v agrees on message m
            case (v, m) =>
              messageWeightMapF(m, dag).map { messageWeightMap =>
                recordAgreement(messageWeightMap, v).map { agreement =>
                  MessageAgreement(m, messageWeightMap, Map(agreement))
                }
              }
          })
          // collect successful agreements (by bonded validators as per message agreed on)
          .map(_.collect { case Some(v) => v })
          // flatten in a single stream
          .map(Stream.emits(_))
          .flatten
      }

    mkAgreementsStream.flatMap {
      // while recording each agreement in agreements map
      _.mapAccumulate(Map.empty[BlockMetadata, WeightMap]) {
        case (acc, MessageAgreement(message, messageWeightMap, stakeAgreed)) =>
          val curVal = acc.getOrElse(message, Map.empty)
          require(
            (stakeAgreed.keySet intersect curVal.keySet).isEmpty,
            s"Logical error during finalization: message ${message.blockHash.show} got duplicate agreement."
          )
          val newVal = curVal ++ stakeAgreed // given requirement above its enough to just sum maps
          (acc.updated(message, newVal), (message, messageWeightMap))
      }
      // output only target message of current agreement
        .map {
          case (fullAgreementsMap, (message, messageWeightMap)) =>
            (message, messageWeightMap, fullAgreementsMap(message))
        }
        // filter only messages that cannot be orphaned
        .filter {
          case (_, messageWeightMap, agreeingWeightMap) =>
            cannotBeOrphaned(messageWeightMap, agreeingWeightMap)
        }
        // compute fault tolerance
        .evalMap {
          case (message, messageWeightMap, agreeingWeightMap) =>
            CliqueOracle
              .computeOutput[F](
                targetMsg = message.blockHash,
                messageWeightMap = messageWeightMap,
                agreeingWeightMap = agreeingWeightMap,
                dag = dag
              )
              .map((message, _))
        }
        // first candidate that meets finalization criteria is new LFB
        .filter { case (_, faultTolerance) => faultTolerance > faultToleranceThreshold }
        .head
        // execute all effects
        .evalMap {
          case (lfb, _) =>
            dag
              .withAncestors(lfb.blockHash, dag.isFinalized(_).not)
              .flatMap(finalisationEffect) >>
              newLfbEffect(lfb.blockHash).as(lfb.blockHash)
        }
        .compile
        .last
    }
  }
}
