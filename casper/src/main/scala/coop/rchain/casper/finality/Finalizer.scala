package coop.rchain.casper.finality

import cats.Show
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.syntax.all._
import coop.rchain.casper.safety.CliqueOracle
import coop.rchain.casper.safety.CliqueOracle.WeightMap
import coop.rchain.dag.{Casper, DagOps, DagReader}
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream

import scala.reflect.ClassTag

/**
  * Block can be recorded as last finalized block (LFB) if Safety oracle outputs fault tolerance (FT)
  * for this block greater then some predefined threshold. This is defined by [[CliqueOracle.computeMaxCliqueWeight]]
  * function, which requires some target block as input arg.
  *
  * Therefore: Finalizer has a scope of search, defined by tips and previous LFB - each of this blocks can be next LFB.
  *
  * We know that LFB advancement is not necessary continuous, next LFB might not be direct child of current one.
  *
  * Therefore: we cannot start from current LFB children and traverse DAG from the bottom to the top, calculating FT
  * for each block. Also its computationally ineffective,
  *
  * BUT we know that scope of search for potential next LFB is constrained. Block A can be finalized only
  * if it has more then half of total stake in bonds map of A translated from tips throughout main parent chain.
  * IMPORTANT: only main parent relation gives weight to potentially finalized block.
  *
  * Therefore: Finalizer should seek for next LFB going through 2 steps:
  *   1. Find messages M in scope of search that have more then half of the stake translated through main parent chain
  *     from tips down to M.
  *   2. Execute [[CliqueOracle.computeMaxCliqueWeight]] on these targets
  *   3. Candidate with highest block number passing finalization threshold become next LFB
  */
object Finalizer {

  /**
    * Candidate for finalization
    * @param message message itself
    * @param localWeightMap weight map as per this message,
    *                       only stake referenced in this map (active stake) is important to this message
    * @param agreeingWeightMap - weight map that agrees on this message, this records active stake translated from tips
    * @tparam M type of message
    * @tparam V type of message creator
    */
  final case class FinalisationCandidate[M, V](
      message: M,
      localWeightMap: WeightMap[V],
      agreeingWeightMap: WeightMap[V],
      height: Long
  ) {
    // if validator is not bonded in the message - translated stake is 0.
    def recordAgreement(validator: V): FinalisationCandidate[M, V] = {
      val stakeAgreed = localWeightMap.getOrElse(validator, 0L)
      this.copy(agreeingWeightMap = agreeingWeightMap.updated(validator, stakeAgreed))
    }

    // if more then half of stake agree on message - it cannot be orphaned
    def cannotBeOrphaned: Boolean = {
      val activeStakeTotal    = localWeightMap.values.sum
      val activeStakeAgreeing = agreeingWeightMap.values.sum
      // in theory if each stake is high enough, e.g. Long.MaxValue, sum of them might result in negative value
      require(activeStakeTotal > 0, message = "Long overflow when computing total stake")
      activeStakeAgreeing > activeStakeTotal.toFloat / 2
    }
  }

  /** Find highest finalized block */
  def run[F[_]: Concurrent: Metrics: Log: Span, M, V: ClassTag](
      dag: DagReader[F, M],
      faultToleranceThreshold: Float,
      lastFinalizedHeight: Long,
      latestMessages: Seq[(V, M)]
  )(
      newLfbEffect: M => F[Unit],
      finalisationEffect: M => F[Unit],
      isFinalised: M => F[Boolean],
      weightMapF: M => F[WeightMap[V]],
      heightF: M => F[Long],
      toJustificationF: M => F[Casper.Justification[M, V]],
      justificationsF: M => F[Set[Casper.Justification[M, V]]]
  )(implicit show: Show[M]): F[Option[M]] =
    for {
      // ref accumulating data about all blocks met on main parent chains while traversing down from tips.
      fcs <- Ref.of[F, Map[M, FinalisationCandidate[M, V]]](Map.empty)
      // run through main parents, record stake translating
      lmProcessing = (m: M, v: V) =>
        for {
          r <- DagOps
                .bfTraverseF(List(m))(
                  m => {
                    for {
                      // weight map as per message view
                      localWM <- CliqueOracle.getCorrespondingWeightMap(m, dag, weightMapF)
                      // height of the message
                      h <- heightF(m)
                      // record this message visited
                      _ <- fcs.update(st => {
                            val init   = FinalisationCandidate(m, localWM, Map.empty[V, Long], h)
                            val curVal = st.getOrElse(m, init)
                            // add to agreeing map of the message validator v
                            st.updated(m, curVal.recordAgreement(v))
                          })
                      // proceed to main parent if not below or sibling of previous LFB
                      mainParent <- dag.mainParent(m)
                      r <- mainParent.traverse(
                            mp =>
                              for {
                                h <- heightF(mp)
                              } yield if (h <= lastFinalizedHeight) List.empty else List(mp)
                          )
                    } yield r.getOrElse(List.empty)
                  }
                )
                .toList
        } yield r
      // run through main parent chain for all latest messages
      work = Stream
        .emits(latestMessages)
        .parEvalMapProcBounded {
          case (v, m) => lmProcessing(m, v)
        }
      _ <- work.compile.drain

      finalizationCandidates <- fcs.get
      finalized <- Stream
                    .fromIterator(
                      finalizationCandidates.values.filter(_.cannotBeOrphaned).toIterator
                    )
                    .parEvalMapProcBounded(
                      c =>
                        CliqueOracle
                          .computeOutput[F, M, V](
                            target = c.message,
                            dag = dag,
                            totalStake = c.localWeightMap.values.sum,
                            agreeingWeightMap = c.agreeingWeightMap,
                            agreeingLatestMessages = latestMessages.filter {
                              case (v, _) => c.agreeingWeightMap.keySet.contains(v)
                            }.toMap,
                            toJustificationF = toJustificationF,
                            justificationsF = justificationsF,
                            heightF = heightF
                          )
                          .map((c, _))
                    )
                    .filter { case (_, ft) => ft > faultToleranceThreshold }
                    .compile
                    .toList
      newLfbOpt = finalized
        .sortBy { case (msg, _) => msg.height }
        .reverse
        .headOption
        .map {
          case (msg, _) => msg.message
        }

      _ <- newLfbOpt.traverse(
            newLFB =>
              for {
                // run effect for new LFB found
                _ <- newLfbEffect(newLFB)
                // finalise new LFB and all ancestors that are not finalized yet
                _ <- dag
                      .withAncestors(newLFB, m => isFinalised(m).not)
                      .map(finalisationEffect)
                      .toList
              } yield ()
          )
    } yield newLfbOpt
}
