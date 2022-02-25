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
  private type V         = Validator    // type for message creator/validator
  private type WeightMap = Map[V, Long] // stakes per message creator

  implicit val baseMetricsSource: Source = coop.rchain.casper.CasperMetricsSource

  /** weight map of main parent (fallbacks to message itself if no parents)
     TODO - why not use local weight map but seek for parent?
    P.S. This is related to the fact that we create latest message for newly bonded validator
    equal to message where bonding deploy has been submitted. So stake from validator that did not create anything is
    put behind this message. So here is one more place where this logic makes things more complex.*/
  def getCorrespondingWeightMap[F[_]: Sync](
      targetMsg: M,
      dag: BlockDagRepresentation[F]
  ): F[WeightMap] =
    dag.lookupUnsafe(targetMsg).flatMap { meta =>
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
      targetMsg: M
  ): F[Boolean] = {
    def mightEventuallyDisagree(lmB: M, lmAjB: M): F[Boolean] =
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
          dag.isInMainChain(targetMsg, j.latestBlockHash)
        }
        // its enough only one disagreement found to declare output false
        r <- disagreements.head.compile.last
      } yield r.isDefined

    // justification from `validatorB` as per latest message of `validatorA`
    val lmAjB = OptionT(dag.latestMessageHash(validatorA)).flatMapF(
      dag
        .lookupUnsafe(_)
        .map(_.justifications.find(_.validator == validatorB).map(_.latestBlockHash))
    )
    val lmB = OptionT(dag.latestMessageHash(validatorB))
    // Check for disagreement, if messages are not found return false
    (lmB, lmAjB).tupled.semiflatMap((mightEventuallyDisagree _).tupled.map(_.not)).getOrElse(false)
  }

  private def computeMaxCliqueWeight[F[_]: Concurrent: Span](
      targetMsg: M,
      agreeingWeightMap: WeightMap,
      dag: BlockDagRepresentation[F]
  ): F[Long] = Span[F].traceI("compute-max-clique-weight") {

    /** across combination of validators compute pairs that do not have disagreement */
    def computeAgreeingValidatorPairs: Stream[F, (V, V)] =
      Stream
        .fromIterator(agreeingWeightMap.keys.toSeq.combinations(2))
        .evalFilter {
          case Seq(a, b) =>
            neverEventuallySeeDisagreement(a, b, dag, targetMsg) &&^
              neverEventuallySeeDisagreement(b, a, dag, targetMsg)
        }
        .map { case Seq(a, b) => (a, b) }

    computeAgreeingValidatorPairs.compile.toList.map { edges =>
      Clique.findMaximumCliqueByWeight[V](edges, agreeingWeightMap)
    }
  }

  def computeOutput[F[_]: Concurrent: Span](
      targetMsg: M,
      messageWeightMap: WeightMap,
      agreeingWeightMap: WeightMap,
      dag: BlockDagRepresentation[F]
  ): F[Float] = {
    val totalStake = messageWeightMap.values.sum.toFloat
    require(totalStake > 0, "Long overflow when computing total stake")
    // if less then 1/2+ of stake agrees on message - it can be orphaned
    if (agreeingWeightMap.values.sum <= totalStake / 2) SafetyOracle.MIN_FAULT_TOLERANCE.pure
    else
      computeMaxCliqueWeight[F](targetMsg, agreeingWeightMap, dag).map { mqw =>
        (mqw * 2 - totalStake) / totalStake
      }
  }

  def normalizedFaultTolerance[F[_]: Concurrent: Log: Metrics: Span](
      targetMsg: M,
      dag: BlockDagRepresentation[F]
  ): F[Float] = {

    /** weight map containing only validators that agree on the the message */
    def agreeingWeightMapF(weightMap: WeightMap): F[WeightMap] = {
      def agree(validator: V, message: M): F[Boolean] =
        dag
          .latestMessageHash(validator)
          .flatMap(_.traverse(dag.isInMainChain(message, _)))
          .map(_.getOrElse(false))

      Stream
        .fromIterator(weightMap.toIterator)
        .evalFilterAsyncProcBounded {
          case (validator, _) => agree(validator, targetMsg)
        }
        .compile
        .to(Map)
    }

    val targetMsgNotInDag = {
      val errStr = s"Fault tolerance for non existing message ${targetMsg.show} requested."
      Log[F].error(errStr).as(SafetyOracle.MIN_FAULT_TOLERANCE)
    }

    val doCompute = {
      val compute = for {
        fullWeightMap     <- getCorrespondingWeightMap(targetMsg, dag)
        agreeingWeightMap <- agreeingWeightMapF(fullWeightMap)
        r                 <- computeOutput[F](targetMsg, fullWeightMap, agreeingWeightMap, dag)
      } yield r

      Log[F].debug(s"Calculating fault tolerance for ${targetMsg.show}.") *>
        Span[F].traceI("normalized-fault-tolerance")(compute)
    }

    dag.contains(targetMsg).ifM(doCompute, targetMsgNotInDag)
  }
}
