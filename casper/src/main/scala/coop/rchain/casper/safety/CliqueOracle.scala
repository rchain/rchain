package coop.rchain.casper.safety

import cats.Show
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all._
import coop.rchain.casper.util.{Clique, ProtoUtil}
import coop.rchain.dag.{Casper, DagOps, DagReader}
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import coop.rchain.metrics.implicits._
import fs2.Stream

import scala.reflect.ClassTag

object CliqueOracle {
  // map of validator id to stake bonded
  type WeightMap[V] = Map[V, Long]

  implicit val baseMetricsSource: Source = coop.rchain.casper.CasperMetricsSource

  private def computeMaxCliqueWeight[F[_]: Concurrent: Span, M, V: ClassTag](
      target: M,
      dag: DagReader[F, M],
      agreeingWeightMap: WeightMap[V],
      agreeingLatestMessages: Map[V, M],
      toJustificationF: M => F[Casper.Justification[M, V]],
      justificationsF: M => F[Set[Casper.Justification[M, V]]],
      heightF: M => F[Long]
  )(implicit show: Show[M]): F[Long] = Span[F].traceI("compute-max-clique-weight") {

    /**
      * Prerequisite for this is that latest messages from a and b both are in main chain with target message
      *
      *     a    b
      *     *    *  <- lmB
      *      \   *
      *       \  *
      *        \ *
      *         \*
      *          *  <- lmAjB
      *
      * 1. get justification of validator b as per latest message of a (lmAjB)
      * 2. check if any self justifications between latest message of b (lmB) and lmAjB are NOT in main chain
      *    with target message. If one found - this is a source of disagreement.
      * */
    def neverEventuallySeeDisagreement(a: V, b: V): F[Boolean] =
      (for {
        // latest messages of validators matched
        lmA <- OptionT.fromOption(agreeingLatestMessages.get(a))
        lmB <- OptionT.fromOption(agreeingLatestMessages.get(b))
        // message of `b` used as a justification by latest message of `a`
        lmAjB <- OptionT(justificationsF(lmA).map(_.find(j => j.creator == b)))

        disagreements = for {
          // self justification of lmAjB or lmAjB itself. Used as a stopper for traversal
          // TODO not completely clear why try to use self justification and not just message itself
          stopper <- justificationsF(lmAjB.message).map(
                      _.find(_.creator == lmAjB.creator)
                        .getOrElse(lmAjB)
                        .message
                    )
          selfJustificationsChain = DagOps
            .bfTraverseF[F, M](List(lmB)) { jFromB =>
              ProtoUtil
                .getCreatorJustification(
                  jFromB,
                  (b: M) => b == stopper,
                  toJustificationF,
                  justificationsF
                )
                .map(_.map(List(_)).getOrElse(List.empty))
            }
          // find if any of traversed selfJustifications is NOT in main chain with target
          r <- selfJustificationsChain
                .filterF(dag.isInMainChain(_, target, heightF).not)
                .flatMap(_.toList)
        } yield r

        r <- OptionT.liftF(disagreements.map(_.isEmpty))

      } yield r).fold(false)(identity)

    /** across combination of validators compute pairs that do not have disagreement */
    def computeAgreeingValidatorPairs: F[List[(V, V)]] = {
      val pairs = agreeingWeightMap.keys.toArray.combinations(2).map { case Array(a, b) => (a, b) }

      Stream
        .fromIterator(pairs)
        .evalFilterAsyncProcBounded {
          case (a, b) =>
            neverEventuallySeeDisagreement(a, b) &&^ neverEventuallySeeDisagreement(b, a)
        }
        .compile
        .toList
    }

    computeAgreeingValidatorPairs.map { edges =>
      Clique.findMaximumCliqueByWeight[V](edges, agreeingWeightMap)
    }
  }

  def normalizedFaultTolerance[F[_]: Concurrent: Log: Metrics: Span, M, V: ClassTag](
      target: M,
      dag: DagReader[F, M],
      getWeightMapF: M => F[WeightMap[V]],
      heightF: M => F[Long],
      latestMessageF: V => F[M],
      toJustificationF: M => F[Casper.Justification[M, V]],
      getJustificationsF: M => F[Set[Casper.Justification[M, V]]]
  )(implicit show: Show[M]): F[Float] = {
    val nonExistingMessage =
      Log[F].error(s"Fault tolerance for non existing message ${target.show} requested.").as(-1f)

    val compute = Span[F].traceI("normalized-fault-tolerance") {
      for {
        // weight map of main parent (fallbacks to message itself if no parents)
        fullWeightMap <- {
          dag
            .mainParent(target)
            .map {
              case Some(mainParent) => mainParent
              case None             => target
            }
        }.flatMap(getWeightMapF)
        // stake that agrees on a target message
        agreeingWeightMap <- fs2.Stream
                              .emits(fullWeightMap.toList)
                              .evalFilterAsyncProcBounded {
                                case (validator, _) =>
                                  for {
                                    lm <- latestMessageF(validator)
                                    r <- dag.isInMainChain(
                                          lm,
                                          target,
                                          heightF
                                        )
                                  } yield r
                              }
                              .compile
                              .toList
                              .map(_.toMap)
        agreeingStake = agreeingWeightMap.values.sum
        // total stake
        totalStake = fullWeightMap.values.sum
        // latest messages of agreeing validators
        agreeingLatestMessages <- agreeingWeightMap.keys.toList
                                   .traverse { k =>
                                     for {
                                       lm <- latestMessageF(k)
                                     } yield (k, lm)
                                   }
                                   .map(_.toMap)
        // compute fault tolerance
        faultToleranceF = computeMaxCliqueWeight[F, M, V](
          target,
          dag,
          agreeingWeightMap,
          agreeingLatestMessages,
          toJustificationF,
          getJustificationsF,
          heightF
        ).map(
          mqw => (mqw * 2 - totalStake).toFloat / totalStake
        )
        // if less then half of stake agrees on message - it can be orphaned
        r <- if (2L * agreeingStake < totalStake) (-1f).pure[F]
            else faultToleranceF
      } yield r
    }

    dag
      .contains(target)
      .ifM(
        Log[F].debug(s"Calculating fault tolerance for ${target.show}.") >> compute,
        nonExistingMessage
      )
  }
}
