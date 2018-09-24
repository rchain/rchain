package coop.rchain.casper

import cats.{Applicative, Foldable, Monad, Now}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.{BlockMessage, Justification}
import coop.rchain.casper.util.{Clique, DagOperations, ProtoUtil}
import coop.rchain.casper.util.ProtoUtil.{mainParent, _}
import coop.rchain.catscontrib.ListContrib

import scala.collection

/*
 * Implementation inspired by Ethereum's CBC casper simulator's Turan oracle implementation.
 *
 * https://github.com/ethereum/cbc-casper/blob/0.2.0/casper/safety_oracles/turan_oracle.py
 *
 * "If nodes in an e-clique see each other agreeing on e and can't see each other disagreeing on e,
 * then there does not exist any new message from inside the clique that will cause them to assign
 * lower scores to e. Further, if the clique has more than half of the validators by weight,
 * then no messages external to the clique can raise the scores these validators assign to
 * a competing estimate to be higher than the score they assign to e."
 *
 * - From https://github.com/ethereum/research/blob/master/papers/CasperTFG/CasperTFG.pdf
 *
 * That is unless there are equivocations.
 * The fault tolerance threshold is a subjective value that the user sets to "secretly" state that they
 * tolerate up to fault_tolerance_threshold fraction of the total weight to equivocate.
 *
 * In the extreme case when your fault tolerance threshold is 1,
 * all validators must be part of the clique that supports the estimate in order to state that it is finalized.
 * If all validators are indeed part of the clique, minMaxCliqueWeight will hopefully be equal to total_weight
 * and is_safe will reduce to total_weight >= total_weight and evaluate to true.
 */
trait SafetyOracle[F[_]] {

  /**
    * The normalizedFaultTolerance must be greater than the fault tolerance threshold t in order
    * for a estimate to be safe.
    *
    * @param estimate Block to detect safety on
    * @return normalizedFaultTolerance float between -1 and 1, where -1 means potentially orphaned
    */
  def normalizedFaultTolerance(blockDag: BlockDag, estimate: BlockMessage): F[Float]
}

object SafetyOracle extends SafetyOracleInstances {
  def apply[F[_]](implicit ev: SafetyOracle[F]): SafetyOracle[F] = ev
}

sealed abstract class SafetyOracleInstances {
  def turanOracle[F[_]: Monad: BlockStore]: SafetyOracle[F] =
    new SafetyOracle[F] {
      def normalizedFaultTolerance(blockDag: BlockDag, estimate: BlockMessage): F[Float] =
        for {
          totalWeight              <- computeTotalWeight(estimate)
          minMaxCliqueWeight       <- computeMinMaxCliqueWeight(blockDag, estimate)
          faultTolerance           = 2 * minMaxCliqueWeight - totalWeight
          normalizedFaultTolerance = faultTolerance.toFloat / totalWeight
        } yield normalizedFaultTolerance

      // To have a maximum clique of half the total weight,
      // you need at least twice the weight of the candidateWeights to be greater than the total weight
      private def computeMinMaxCliqueWeight(blockDag: BlockDag, estimate: BlockMessage): F[Long] =
        for {
          candidateWeights <- computeCandidateWeights(blockDag, estimate)
          totalWeight      <- computeTotalWeight(estimate)
          minMaxCliqueWeight <- if (2L * candidateWeights.values.sum < totalWeight) {
                                 0L.pure[F]
                               } else {
                                 val vertexCount = candidateWeights.keys.size
                                 for {
                                   edgeCount <- agreementGraphEdgeCount(
                                                 blockDag,
                                                 estimate,
                                                 candidateWeights
                                               )
                                 } yield
                                   minTotalValidatorWeight(
                                     estimate,
                                     maxCliqueMinSize(vertexCount, edgeCount)
                                   )
                               }
        } yield minMaxCliqueWeight

      private def computeTotalWeight(estimate: BlockMessage): F[Long] =
        for {
          mainParentWeightMap <- computeMainParentWeightMap(estimate)
        } yield weightMapTotal(mainParentWeightMap)

      private def computeCandidateWeights(
          blockDag: BlockDag,
          estimate: BlockMessage
      ): F[Map[Validator, Long]] =
        for {
          weights <- computeMainParentWeightMap(estimate)
          candidateWeights <- weights.toList.traverse {
                               case (validator, stake) =>
                                 val maybeLatestMessage = blockDag.latestMessages.get(validator)
                                 maybeLatestMessage match {
                                   case Some(latestMessage) =>
                                     for {
                                       isCompatible <- computeCompatibility(estimate, latestMessage)
                                       result = if (isCompatible) {
                                         Some((validator, stake))
                                       } else {
                                         none[(Validator, Long)]
                                       }
                                     } yield result
                                   case None =>
                                     none[(Validator, Long)].pure[F]
                                 }
                             }
        } yield candidateWeights.flatten.toMap

      private def computeMainParentWeightMap(estimate: BlockMessage): F[Map[BlockHash, Long]] =
        for {
          estimateMainParent <- mainParent[F](estimate)
          mainParentWeightMap = estimateMainParent match {
            case Some(parent) => weightMap(parent)
            case None         => weightMap(estimate) // Genesis
          }
        } yield mainParentWeightMap

      private def findMaximumClique(
          edges: List[(Validator, Validator)],
          candidates: Map[Validator, Long]
      ): (List[Validator], Long) =
        Clique
          .findCliquesRecursive(edges)
          .foldLeft((List[Validator](), 0L)) {
            case ((maxClique, maxWeight), clique) => {
              val weight = clique.map(candidates.getOrElse(_, 0L)).sum
              if (weight > maxWeight) {
                (clique, weight)
              } else if (weight == maxWeight && clique.size > maxClique.size) {
                (clique, weight)
              } else {
                (maxClique, maxWeight)
              }
            }
          }

      private def agreementGraphEdgeCount(
          blockDag: BlockDag,
          estimate: BlockMessage,
          candidates: Map[Validator, Long]
      ): F[Int] = {
        def findAgreeingJustificationHash(
            justificationHashes: List[BlockHash],
            validator: Validator
        ): F[Option[BlockHash]] =
          ListContrib.findM(
            justificationHashes,
            justificationHash =>
              for {
                justificationBlock <- unsafeGetBlock[F](justificationHash)
                isSenderSecond     = justificationBlock.sender == validator
                compatible         <- computeCompatibility(estimate, justificationBlock)
              } yield isSenderSecond && compatible
          )

        def seesAgreement(first: Validator, second: Validator): F[Boolean] = {
          val maybeFirstLatest = blockDag.latestMessages.get(first)
          maybeFirstLatest match {
            case Some(firstLatestBlock) =>
              for {
                justificationHashes <- firstLatestBlock.justifications
                                        .map(_.latestBlockHash)
                                        .pure[F]
                agreeingJustificationHash <- findAgreeingJustificationHash(
                                              justificationHashes.toList,
                                              second
                                            )
              } yield agreeingJustificationHash.isDefined
            case None => false.pure[F]
          }
        }

        def filterChildren(candidate: BlockMessage, validator: Validator): F[List[BlockMessage]] =
          blockDag.latestMessages.get(validator) match {
            case Some(latestMessageByValidator) =>
              DagOperations
                .bfTraverseF[F, BlockMessage](List(latestMessageByValidator)) { block =>
                  ProtoUtil.getCreatorJustificationAsList[F](block, validator, b => b == candidate)
                }
                .filter { potentialChild =>
                  val validatorCreatedChild = potentialChild.sender == validator
                  val isFutureBlock         = candidate.seqNum <= potentialChild.seqNum
                  validatorCreatedChild && isFutureBlock
                }
                .toList
            case None => List.empty[BlockMessage].pure[F]
          }

        def neverEventuallySeeDisagreement(first: Validator, second: Validator): F[Boolean] = {
          val maybeFirstLatest = blockDag.latestMessages.get(first)
          maybeFirstLatest match {
            case Some(firstLatestBlock) =>
              for {
                justificationHashes <- firstLatestBlock.justifications
                                        .map(_.latestBlockHash)
                                        .pure[F]
                justificationBlockSecondList <- justificationHashes.toList.traverse(
                                                 justificationHash =>
                                                   for {
                                                     justificationBlock <- unsafeGetBlock[F](
                                                                            justificationHash
                                                                          )
                                                     isSenderSecond = justificationBlock.sender == second
                                                     result = if (isSenderSecond) {
                                                       Some(justificationBlock)
                                                     } else {
                                                       none[BlockMessage]
                                                     }
                                                   } yield result
                                               )
                _                        = assert(justificationBlockSecondList.flatten.length == 1)
                justificationBlockSecond = justificationBlockSecondList.flatten.head
                potentialDisagreements   <- filterChildren(justificationBlockSecond, second)
                result <- potentialDisagreements.forallM { potentialDisagreement =>
                           computeCompatibility(estimate, potentialDisagreement)
                         }
              } yield result
            case None => false.pure[F]
          }
        }

        def computeAgreementGraphEdges: F[List[(Validator, Validator)]] =
          (for {
            x <- candidates.keys
            y <- candidates.keys
            if x.toString > y.toString // TODO: Order ByteString
          } yield (x, y)).toList.filterA {
            case (first: Validator, second: Validator) =>
              // TODO: Replace with equivalent of <&&>
              Monad[F].ifM(seesAgreement(first, second))(
                Monad[F].ifM(seesAgreement(second, first))(
                  Monad[F].ifM(neverEventuallySeeDisagreement(first, second))(
                    Monad[F].ifM(neverEventuallySeeDisagreement(second, first))(
                      true.pure[F],
                      false.pure[F]
                    ),
                    false.pure[F]
                  ),
                  false.pure[F]
                ),
                false.pure[F]
              )
          }

        for {
          edges <- computeAgreementGraphEdges
        } yield findMaximumClique(edges, candidates)._1.size
      }

      // TODO: Change to isInBlockDAG
      private def computeCompatibility(candidate: BlockMessage, target: BlockMessage): F[Boolean] =
        isInMainChain[F](candidate, target)

      // See Turan's theorem (https://en.wikipedia.org/wiki/Tur%C3%A1n%27s_theorem)
      private def maxCliqueMinSize(vertices: Int, edges: Int) = {
        val verticesSquared = vertices * vertices
        math.ceil(verticesSquared.toDouble / (verticesSquared - 2 * edges).toDouble).toInt
      }
    }
}
