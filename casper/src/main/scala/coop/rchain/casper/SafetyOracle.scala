package coop.rchain.casper

import cats.{Applicative, Foldable, Monad, Now}
import cats.Id
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import cats.syntax.option._
import coop.rchain.blockstorage.BlockMetadata
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.{Clique, DagOperations, ProtoUtil}

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
  def normalizedFaultTolerance(blockDag: BlockDag, estimateBlockHash: BlockHash): Float
}

object SafetyOracle extends SafetyOracleInstances {
  def apply[F[_]](implicit ev: SafetyOracle[F]): SafetyOracle[F] = ev
}

sealed abstract class SafetyOracleInstances {
  def turanOracle[F[_]: Monad]: SafetyOracle[F] =
    new SafetyOracle[F] {
      def normalizedFaultTolerance(blockDag: BlockDag, estimateBlockHash: BlockHash): Float = {
        val totalWeight        = computeTotalWeight(blockDag, estimateBlockHash)
        val minMaxCliqueWeight = computeMinMaxCliqueWeight(blockDag, estimateBlockHash)
        val faultTolerance     = 2 * minMaxCliqueWeight - totalWeight
        faultTolerance.toFloat / totalWeight
      }

      // To have a maximum clique of half the total weight,
      // you need at least twice the weight of the candidateWeights to be greater than the total weight
      private def computeMinMaxCliqueWeight(
          blockDag: BlockDag,
          estimateBlockHash: BlockHash
      ): Long = {
        val candidateWeights = computeCandidateWeights(blockDag, estimateBlockHash)
        val totalWeight      = computeTotalWeight(blockDag, estimateBlockHash)
        if (2L * candidateWeights.values.sum < totalWeight) {
          0L
        } else {
          val vertexCount = candidateWeights.keys.size
          val edgeCount   = agreementGraphEdgeCount(blockDag, estimateBlockHash, candidateWeights)
          minTotalValidatorWeight(
            blockDag,
            estimateBlockHash,
            maxCliqueMinSize(vertexCount, edgeCount)
          )
        }
      }

      private def computeTotalWeight(blockDag: BlockDag, estimateBlockHash: BlockHash): Long = {
        val mainParentWeightMap = computeMainParentWeightMap(blockDag, estimateBlockHash)
        weightMapTotal(mainParentWeightMap)
      }

      private def computeCandidateWeights(
          blockDag: BlockDag,
          estimateBlockHash: BlockHash
      ): Map[Validator, Long] = {
        val weights = computeMainParentWeightMap(blockDag, estimateBlockHash)
        val candidateWeights = weights.toList.flatMap {
          case (validator, stake) =>
            val maybeLatestMessage = blockDag.latestMessages.get(validator)
            maybeLatestMessage match {
              case Some(latestMessage) =>
                val isCompatible =
                  computeCompatibility(blockDag, estimateBlockHash, latestMessage.blockHash)
                if (isCompatible) {
                  Some((validator, stake))
                } else {
                  none[(Validator, Long)]
                }
              case None =>
                none[(Validator, Long)]
            }
        }
        candidateWeights.toMap
      }

      private def computeMainParentWeightMap(
          blockDag: BlockDag,
          estimateBlockHash: BlockHash
      ): Map[BlockHash, Long] =
        blockDag.dataLookup(estimateBlockHash).parents.headOption match {
          case Some(parent) => blockDag.dataLookup(parent).weightMap
          case None         => blockDag.dataLookup(estimateBlockHash).weightMap
        }

      private def findMaximumClique(
          edges: List[(Validator, Validator)],
          candidates: Map[Validator, Long]
      ): (List[Validator], Long) =
        Clique
          .findCliquesRecursive(edges)
          .foldLeft((List[Validator](), 0L)) {
            case ((maxClique, maxWeight), clique) =>
              val weight = clique.map(candidates.getOrElse(_, 0L)).sum
              if (weight > maxWeight) {
                (clique, weight)
              } else if (weight == maxWeight && clique.size > maxClique.size) {
                (clique, weight)
              } else {
                (maxClique, maxWeight)
              }
          }

      private def agreementGraphEdgeCount(
          blockDag: BlockDag,
          estimateBlockHash: BlockHash,
          candidates: Map[Validator, Long]
      ): Int = {
        def findAgreeingJustificationHash(
            justificationHashes: List[BlockHash],
            validator: Validator
        ): Option[BlockHash] =
          justificationHashes.find(justificationHash => {
            blockDag.dataLookup.get(justificationHash) match {
              case Some(justificationMetadata) =>
                val compatible =
                  computeCompatibility(blockDag, estimateBlockHash, justificationHash)
                compatible && justificationMetadata.sender == validator
              case None => false
            }
          })

        def seesAgreement(first: Validator, second: Validator): Boolean = {
          val maybeFirstLatest = blockDag.latestMessages.get(first)
          maybeFirstLatest match {
            case Some(firstLatestBlock) =>
              val justificationHashes = firstLatestBlock.justifications.map(_.latestBlockHash)
              val agreeingJustificationHash = findAgreeingJustificationHash(
                justificationHashes.toList,
                second
              )
              agreeingJustificationHash.isDefined
            case None => false
          }
        }

        def filterChildren(candidate: BlockMetadata, validator: Validator): List[BlockHash] =
          blockDag.latestMessages.get(validator) match {
            case Some(latestMessageByValidator) =>
              DagOperations
                .bfTraverseF[Id, BlockHash](List(latestMessageByValidator.blockHash)) { blockHash =>
                  ProtoUtil.getCreatorJustificationAsListByInMemory(
                    blockDag,
                    blockHash,
                    validator,
                    b => b == candidate.blockHash
                  )
                }
                .filter(potentialChild => {
                  val metadata              = blockDag.dataLookup(potentialChild)
                  val isFutureBlock         = candidate.seqNum <= metadata.seqNum
                  val validatorCreatedChild = metadata.sender == validator
                  validatorCreatedChild && isFutureBlock
                })
                .toList
            case None => List.empty[BlockHash]
          }

        def neverEventuallySeeDisagreement(first: Validator, second: Validator): Boolean = {
          val maybeFirstLatest = blockDag.latestMessages.get(first)
          maybeFirstLatest match {
            case Some(firstLatestBlock) =>
              val justificationHashes = firstLatestBlock.justifications.map(_.latestBlockHash)
              val justificationBlockSecondList = justificationHashes.toList
                .flatMap(
                  justificationHash =>
                    blockDag.dataLookup
                      .get(justificationHash)
                      .filter(_.sender == second)
                )
              assert(justificationBlockSecondList.length == 1)
              val justificationBlockSecond = justificationBlockSecondList.head
              val potentialDisagreements   = filterChildren(justificationBlockSecond, second)
              potentialDisagreements.forall(
                potentialDisagreement =>
                  computeCompatibility(blockDag, estimateBlockHash, potentialDisagreement)
              )
            case None => false
          }
        }

        def computeAgreementGraphEdges: List[(Validator, Validator)] =
          (for {
            x <- candidates.keys
            y <- candidates.keys
            if x.toString > y.toString // TODO: Order ByteString
          } yield (x, y)).toList.filter {
            case (first: Validator, second: Validator) =>
              seesAgreement(first, second) && seesAgreement(second, first) &&
                neverEventuallySeeDisagreement(first, second) &&
                neverEventuallySeeDisagreement(second, first)
          }

        val edges = computeAgreementGraphEdges
        findMaximumClique(edges, candidates)._1.size
      }

      // TODO: Change to isInBlockDAG
      private def computeCompatibility(
          blockDag: BlockDag,
          candidateBlockHash: BlockHash,
          targetBlockHash: BlockHash
      ): Boolean =
        isInMainChain(blockDag, candidateBlockHash, targetBlockHash)

      // See Turan's theorem (https://en.wikipedia.org/wiki/Tur%C3%A1n%27s_theorem)
      private def maxCliqueMinSize(vertices: Int, edges: Int) = {
        val verticesSquared = vertices * vertices
        math.ceil(verticesSquared.toDouble / (verticesSquared - 2 * edges).toDouble).toInt
      }
    }
}
