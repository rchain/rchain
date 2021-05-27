package coop.rchain.casper.safety

import cats.Show
import cats.data.OptionT
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.casper.util.{Clique, ProtoUtil}
import coop.rchain.dag.{DagOps, DagReader}
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream

object CliqueOracle {

  final case class Justification[M, V](message: M, creator: V)

  // map of validator id to stake bonded
  type WeightMap[V] = Map[V, Long]

  implicit val baseMetricsSource: Source = coop.rchain.casper.CasperMetricsSource

  private def computeMaxCliqueWeight[F[_]: Concurrent: Span, M, V](
      target: M,
      dag: DagReader[F, M],
      agreeingWeightMap: WeightMap[V],
      agreeingLatestMessages: Map[V, M],
      toJustificationF: M => F[Justification[M, V]],
      justificationsF: M => F[Set[Justification[M, V]]],
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
    def neverEventuallySeeDisagreement(a: V, b: V): F[Boolean] = {
      val noDisagreement = for {
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

      } yield r

      noDisagreement.getOrElse(false)
    }

    /** across combination of validators compute pairs that do not have disagreement */
    def computeAgreeingValidatorPairs: Stream[F, (V, V)] = {
      val validatorCombinations = agreeingWeightMap.keys.toSeq.combinations(2)
      Stream
        .fromIterator(validatorCombinations)
        .evalFilterAsyncProcBounded {
          case Seq(a, b) =>
            neverEventuallySeeDisagreement(a, b) &&^ neverEventuallySeeDisagreement(b, a)
        }
        .map { case Seq(a, b) => (a, b) }
    }

    computeAgreeingValidatorPairs.compile.toList.map { edges =>
      Clique.findMaximumCliqueByWeight[V](edges, agreeingWeightMap)
    }
  }

  def computeOutput[F[_]: Concurrent: Span, M, V](
      target: M,
      dag: DagReader[F, M],
      totalStake: Long,
      agreeingWeightMap: WeightMap[V],
      agreeingLatestMessages: Map[V, M],
      toJustificationF: M => F[Justification[M, V]],
      justificationsF: M => F[Set[Justification[M, V]]],
      heightF: M => F[Long]
  )(implicit show: Show[M]): F[Float] =
    // if less then 1/2+ of stake agrees on message - it can be orphaned
    if (agreeingWeightMap.values.sum <= totalStake.toFloat / 2) (-1f).pure[F]
    else {
      computeMaxCliqueWeight[F, M, V](
        target,
        dag,
        agreeingWeightMap,
        agreeingLatestMessages,
        toJustificationF,
        justificationsF,
        heightF
      ).map(
        mqw => (mqw * 2 - totalStake).toFloat / totalStake
      )
    }

  /** weight map of main parent (fallbacks to message itself if no parents)
     TODO - why not use local weight map but seek for parent?
    P.S. This is directly related to the fact that we create latest message for newly bonded validator
    equal to message where bonding deploy has been submitted. So stake from validator that did not create anything is
    put behind this message. So here is one more place where this logic makes things more complex.*/
  def getCorrespondingWeightMap[F[_]: Sync, M, V](
      target: M,
      dag: DagReader[F, M],
      getWeightMapF: M => F[WeightMap[V]]
  ): F[WeightMap[V]] =
    dag
      .mainParent(target)
      .map {
        case Some(mainParent) => mainParent
        case None             => target
      }
      .flatMap(getWeightMapF)

  def normalizedFaultTolerance[F[_]: Concurrent: Log: Metrics: Span, M, V](
      target: M,
      dag: DagReader[F, M],
      weightMapF: M => F[WeightMap[V]],
      heightF: M => F[Long],
      latestMessageF: V => F[M],
      toJustificationF: M => F[Justification[M, V]],
      getJustificationsF: M => F[Set[Justification[M, V]]]
  )(implicit show: Show[M]): F[Float] = {
    val nonExistingMessage =
      Log[F].error(s"Fault tolerance for non existing message ${target.show} requested.").as(-1f)

    val messageWeightMapF = (m: M) => CliqueOracle.getCorrespondingWeightMap(m, dag, weightMapF)

    /** if validator agree on message by main chain */
    def agree(validator: V, message: M): F[Boolean] =
      latestMessageF(validator).flatMap(dag.isInMainChain(_, message, heightF))

    def agreeingWeightMapF(weightMap: WeightMap[V]): F[WeightMap[V]] =
      fs2.Stream
        .fromIterator(weightMap.toIterator)
        .evalFilterAsyncProcBounded {
          case (validator, _) => agree(validator, target)
        }
        .compile
        .to(Map)

    val compute = for {
      fullWeightMap <- messageWeightMapF(target)
      // stake that agrees on a target message
      agreeingWeightMap <- agreeingWeightMapF(fullWeightMap)
      // total stake
      totalStake = fullWeightMap.values.sum
      _ <- new ArithmeticException("Long overflow when computing total stake").raiseError
            .whenA(totalStake < 0)
      // latest messages of agreeing validators
      agreeingLatestMessages <- agreeingWeightMap.keys.toList
                                 .traverse { k =>
                                   for {
                                     lm <- latestMessageF(k)
                                   } yield (k, lm)
                                 }
                                 .map(_.toMap)
      r <- computeOutput[F, M, V](
            target,
            dag,
            totalStake,
            agreeingWeightMap,
            agreeingLatestMessages,
            toJustificationF,
            getJustificationsF,
            heightF
          )
    } yield r

    dag
      .contains(target)
      .ifM(
        Log[F].debug(s"Calculating fault tolerance for ${target.show}.") >>
          Span[F].traceI("normalized-fault-tolerance")(compute),
        nonExistingMessage
      )
  }
}
