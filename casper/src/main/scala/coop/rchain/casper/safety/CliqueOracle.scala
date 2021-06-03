package coop.rchain.casper.safety

import cats.data.OptionT
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.util.Clique
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.implicits._
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import fs2.Stream

object CliqueOracle {

  private type M         = BlockHash    // type for message
  private type V         = Validator    // type for message creator
  private type WeightMap = Map[V, Long] // stakes per message creator

  implicit val baseMetricsSource: Source = coop.rchain.casper.CasperMetricsSource

  /** weight map of main parent (fallbacks to message itself if no parents)
     TODO - why not use local weight map but seek for parent?
    P.S. This is related to the fact that we create latest message for newly bonded validator
    equal to message where bonding deploy has been submitted. So stake from validator that did not create anything is
    put behind this message. So here is one more place where this logic makes things more complex.*/
  private def getCorrespondingWeightMap[F[_]: Sync](
      target: M,
      dag: BlockDagRepresentation[F]
  ): F[WeightMap] =
    dag.lookupUnsafe(target).flatMap { meta =>
      meta.parents.headOption match {
        case Some(mainParent) => dag.lookupUnsafe(mainParent).map(_.weightMap)
        case None             => meta.weightMap.pure
      }
    }

  /**
    * If two validators will never have disagreement on target message
    *
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
    *    with target message.
    *
    *    If one found - this is a source of disagreement.
    * */
  private def neverEventuallySeeDisagreement[F[_]: Sync](
      validatorA: V,
      validatorB: V,
      dag: BlockDagRepresentation[F],
      targetMessage: M
  ): F[Boolean] = {
    def mightEventuallyDisagree(lmB: BlockHash, lmAjB: BlockHash): F[Boolean] =
      for {
        // self justification of lmAjB or lmAjB itself. Used as a stopper for traversal
        // TODO not completely clear why try to use self justification and not just message itself
        stopper <- dag.selfJustification(lmAjB).map(_.map(_.latestBlockHash).getOrElse(lmAjB))
        // stream of self justifications till stopper
        lmBSelfJustifications = dag
          .selfJustificationChain(lmB)
          .takeWhile(_.latestBlockHash != stopper)
        // if message is not in main chain with target - this is disagreement
        disagreements = lmBSelfJustifications.evalFilterNot { j =>
          dag.isInMainChain(targetMessage, j.latestBlockHash)
        }
        // its enough only one disagreement found to declare output false
        r <- disagreements.head.compile.last
      } yield r.isDefined

    val noDisagreement = for {
      lmA <- OptionT(dag.latestMessageHash(validatorA))
      lmB <- OptionT(dag.latestMessageHash(validatorB))
      // justification from `validatorB` as per latest message of `validatorA`
      lmAjBF = dag.lookupUnsafe(lmA).map {
        _.justifications.find(j => j.validator == validatorB).map(_.latestBlockHash)
      }
      lmAjB <- OptionT(lmAjBF)
      r     <- OptionT.liftF(mightEventuallyDisagree(lmB, lmAjB).not)
    } yield r

    noDisagreement.getOrElse(false)
  }

  private def computeMaxCliqueWeight[F[_]: Concurrent: Span](
      target: M,
      agreeingWeightMap: WeightMap,
      dag: BlockDagRepresentation[F]
  ): F[Long] = Span[F].traceI("compute-max-clique-weight") {

    /** across combination of validators compute pairs that do not have disagreement */
    def computeAgreeingValidatorPairs: Stream[F, (V, V)] =
      Stream
        .fromIterator(agreeingWeightMap.keys.toSeq.combinations(2))
        .evalFilter {
          case Seq(a, b) =>
            neverEventuallySeeDisagreement(a, b, dag, target) &&^
              neverEventuallySeeDisagreement(b, a, dag, target)
        }
        .map { case Seq(a, b) => (a, b) }

    computeAgreeingValidatorPairs.compile.toList.map { edges =>
      Clique.findMaximumCliqueByWeight[V](edges, agreeingWeightMap)
    }
  }

  def computeOutput[F[_]: Concurrent: Span](
      target: M,
      messageWeightMap: WeightMap,
      agreeingWeightMap: WeightMap,
      dag: BlockDagRepresentation[F]
  ): F[Float] = {
    val totalStake = messageWeightMap.values.sum.toFloat
    require(totalStake > 0, "Long overflow when computing total stake")
    // if less then 1/2+ of stake agrees on message - it can be orphaned
    if (agreeingWeightMap.values.sum <= totalStake / 2) SafetyOracle.MIN_FAULT_TOLERANCE.pure
    else
      computeMaxCliqueWeight[F](target, agreeingWeightMap, dag).map { mqw =>
        (mqw * 2 - totalStake) / totalStake
      }
  }

  def normalizedFaultTolerance[F[_]: Concurrent: Log: Metrics: Span](
      target: M,
      dag: BlockDagRepresentation[F]
  ): F[Float] = {

    /** weight map containing only validators that agree on the the message */
    def agreeingWeightMapF(weightMap: WeightMap): F[WeightMap] = {
      def agree(validator: V, message: M): F[Boolean] =
        dag
          .latestMessageHash(validator)
          .flatMap(_.traverse(dag.isInMainChain(message, _)))
          .map(_.getOrElse(false))

      fs2.Stream
        .fromIterator(weightMap.toIterator)
        .evalFilterAsyncProcBounded {
          case (validator, _) => agree(validator, target)
        }
        .compile
        .to(Map)
    }

    val targetNotInDag = {
      val msg = s"Fault tolerance for non existing message ${target.show} requested."
      Log[F].error(msg).as(SafetyOracle.MIN_FAULT_TOLERANCE)
    }

    val doCompute = {
      val compute = for {
        fullWeightMap     <- getCorrespondingWeightMap(target, dag)
        agreeingWeightMap <- agreeingWeightMapF(fullWeightMap)
        r                 <- computeOutput[F](target, fullWeightMap, agreeingWeightMap, dag)
      } yield r

      Log[F].debug(s"Calculating fault tolerance for ${target.show}.") *>
        Span[F].traceI("normalized-fault-tolerance")(compute)
    }

    dag.contains(target).ifM(doCompute, targetNotInDag)
  }
}
