package coop.rchain.casper

import cats.Monad
import cats.syntax.all._
import cats.instances.list._
import coop.rchain.catscontrib._
import Catscontrib._
import cats.data.OptionT
import cats.effect.Sync
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol.Justification
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.{Clique, ProtoUtil}
import coop.rchain.models.BlockMetadata
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.shared.{DagOps, Log, StreamT}

/*
 * Implementation inspired by Ethereum's CBC casper simulator's clique oracle implementation.
 *
 * https://github.com/ethereum/cbc-casper/blob/0.2.0/casper/safety_oracles/clique_oracle.py
 *
 * "If nodes in an e-clique see each other agreeing on e and can't see each other disagreeing on e,
 * then there does not exist any new message from inside the clique that will cause them to assign
 * lower scores to e. Further, if the clique has more than half of the validators by weight,
 * then no messages external to the clique can raise the scores these validators assign to
 * a competing [candidate] to be higher than the score they assign to e."
 *
 * - From https://github.com/ethereum/research/blob/master/papers/CasperTFG/CasperTFG.pdf
 *
 * That is unless there are equivocations.
 * The fault tolerance threshold is a subjective value that the user sets to "secretly" state that they
 * tolerate up to fault_tolerance_threshold fraction of the total weight to equivocate.
 *
 * In the extreme case when your normalized fault tolerance threshold is 1,
 * all validators must be part of the clique that supports the candidate in order to state that it is finalized.
 */
trait SafetyOracle[F[_]] {

  /**
    * The normalizedFaultTolerance must be greater than the fault tolerance threshold t in order
    * for a candidate to be safe.
    *
    * @param candidateBlockHash Block hash of candidate block to detect safety on
    * @return normalizedFaultTolerance float between -1 and 1, where -1 means potentially orphaned
    */
  def normalizedFaultTolerance(
      blockDag: BlockDagRepresentation[F],
      candidateBlockHash: BlockHash
  ): F[Float]
}

object SafetyOracle extends SafetyOracleInstances {
  def apply[F[_]](implicit ev: SafetyOracle[F]): SafetyOracle[F] = ev
}

sealed abstract class SafetyOracleInstances {
  def cliqueOracle[F[_]: Sync: Log: Metrics: Span]: SafetyOracle[F] =
    new SafetyOracle[F] {
      private val SafetyOracleMetricsSource: Metrics.Source =
        Metrics.Source(CasperMetricsSource, "safety-oracle")

      /**
        * To have a maximum clique of half the total weight,
        * you need at least twice the weight of the agreeingValidatorToWeight to be greater than the total weight.
        * If that is false, we don't need to compute agreementGraphMaxCliqueWeight
        * as we know the value is going to be below 0 and thus useless for finalization.
        */
      def normalizedFaultTolerance(
          blockDag: BlockDagRepresentation[F],
          candidateBlockHash: BlockHash
      ): F[Float] = Span[F].trace(SafetyOracleMetricsSource) {
        for {
          _ <- Log[F].debug(
                s"Calculating faultTolearance of block ${PrettyPrinter.buildString(candidateBlockHash)} "
              )
          maybeCandidateMetadata <- blockDag.lookup(candidateBlockHash)
          faultTolerance <- maybeCandidateMetadata match {
                             case Some(candidateMetadata) =>
                               for {
                                 totalWeight <- computeTotalWeight(blockDag, candidateMetadata)
                                 _           <- Span[F].mark("total-weight")
                                 agreeingValidatorToWeight <- computeAgreeingValidatorToWeight(
                                                               blockDag,
                                                               candidateMetadata
                                                             )
                                 _ <- Span[F].mark("agreeing-validator-to-weight")
                                 maxCliqueWeight <- if (2L * agreeingValidatorToWeight.values.sum < totalWeight) {
                                                     0L.pure[F]
                                                   } else {
                                                     agreementGraphMaxCliqueWeight(
                                                       blockDag,
                                                       candidateMetadata,
                                                       agreeingValidatorToWeight
                                                     )
                                                   }
                                 _ <- Span[F].mark("max-clique-weight")

                                 faultTolerance = 2 * maxCliqueWeight - totalWeight
                               } yield faultTolerance.toFloat / totalWeight
                             // if the node can to find the block, it would return -1 and regard that block as orphaned
                             case None =>
                               Log[F].info(
                                 s"calculate faultTolerance blockHash ${PrettyPrinter.buildString(candidateBlockHash)} failed because it can not be found in store."
                               ) >> (-1L).toFloat
                                 .pure[F]
                           }
        } yield faultTolerance
      }

      private def computeTotalWeight(
          blockDag: BlockDagRepresentation[F],
          candidateMetadata: BlockMetadata
      ): F[Long] =
        computeMainParentWeightMap(blockDag, candidateMetadata).map(weightMapTotal)

      private def computeAgreeingValidatorToWeight(
          blockDag: BlockDagRepresentation[F],
          candidateMetadata: BlockMetadata
      ): F[Map[Validator, Long]] =
        for {
          weights <- computeMainParentWeightMap(blockDag, candidateMetadata)
          agreeingWeights <- weights.toList.traverse {
                              case (validator, stake) =>
                                blockDag.latestMessageHash(validator).flatMap {
                                  case Some(latestMessageHash) =>
                                    computeCompatibility(
                                      blockDag,
                                      candidateMetadata,
                                      latestMessageHash
                                    ).map { isCompatible =>
                                      if (isCompatible) {
                                        Some((validator, stake))
                                      } else {
                                        none[(Validator, Long)]
                                      }
                                    }
                                  case None =>
                                    none[(Validator, Long)].pure[F]
                                }
                            }
        } yield agreeingWeights.flatten.toMap

      private def computeMainParentWeightMap(
          blockDag: BlockDagRepresentation[F],
          candidateMetadata: BlockMetadata
      ): F[Map[BlockHash, Long]] =
        candidateMetadata.parents.headOption match {
          case Some(parent) =>
            blockDag.lookup(parent).map(_.fold(candidateMetadata.weightMap)(_.weightMap))
          case None => candidateMetadata.weightMap.pure[F]
        }

      private def agreementGraphMaxCliqueWeight(
          blockDag: BlockDagRepresentation[F],
          candidateMetadata: BlockMetadata,
          agreeingValidatorToWeight: Map[Validator, Long]
      ): F[Long] = {
        def filterChildren(block: BlockMetadata, validator: Validator): F[StreamT[F, BlockHash]] =
          blockDag.latestMessageHash(validator).flatMap {
            case Some(latestByValidatorHash) =>
              val creatorJustificationOrGenesis = block.justifications
                .find(_.validator == block.sender)
                .fold(block.blockHash)(_.latestBlockHash)
              DagOps
                .bfTraverseF[F, BlockHash](List(latestByValidatorHash)) { blockHash =>
                  ProtoUtil.getCreatorJustificationAsListUntilGoalInMemory(
                    blockDag,
                    blockHash,
                    b => b == creatorJustificationOrGenesis
                  )
                }
                .pure[F]
            case None => StreamT.empty[F, BlockHash].pure[F]
          }

        def neverEventuallySeeDisagreement(
            first: Validator,
            second: Validator
        ): F[Boolean] =
          (for {
            firstLatestBlock <- OptionT(blockDag.latestMessage(first))
            secondLatestOfFirstLatestHash <- OptionT.fromOption[F](
                                              firstLatestBlock.justifications
                                                .find {
                                                  case Justification(validator, _) =>
                                                    validator == second
                                                }
                                                .map(_.latestBlockHash)
                                            )
            secondLatestOfFirstLatest <- OptionT(blockDag.lookup(secondLatestOfFirstLatestHash))
            potentialDisagreements <- OptionT.liftF(
                                       filterChildren(secondLatestOfFirstLatest, second)
                                     )
            // TODO: Implement forallM on StreamT
            result <- OptionT.liftF(potentialDisagreements.toList.flatMap(_.forallM {
                       potentialDisagreement =>
                         computeCompatibility(blockDag, candidateMetadata, potentialDisagreement)
                     }))
          } yield result).fold(false)(identity)

        def computeAgreementGraphEdges: F[List[(Validator, Validator)]] =
          (for {
            x <- agreeingValidatorToWeight.keys
            y <- agreeingValidatorToWeight.keys
            if x.toString > y.toString // TODO: Order ByteString
          } yield (x, y)).toList.filterA {
            case (first: Validator, second: Validator) =>
              neverEventuallySeeDisagreement(first, second) &&^ neverEventuallySeeDisagreement(
                second,
                first
              )
          }

        computeAgreementGraphEdges.map { edges =>
          Clique.findMaximumCliqueByWeight[Validator](edges, agreeingValidatorToWeight)
        }
      }

      private def computeCompatibility(
          blockDag: BlockDagRepresentation[F],
          candidateMetadata: BlockMetadata,
          targetBlockHash: BlockHash
      ): F[Boolean] =
        isInMainChain(blockDag, candidateMetadata, targetBlockHash)
    }
}
