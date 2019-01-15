package coop.rchain.casper

import cats.Monad
import cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockMetadata}
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.{Clique, DagOperations, ProtoUtil}
import coop.rchain.shared.Log

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
  def cliqueOracle[F[_]: Monad: Log]: SafetyOracle[F] =
    new SafetyOracle[F] {

      /**
        * To have a maximum clique of half the total weight,
        * you need at least twice the weight of the agreeingValidatorToWeight to be greater than the total weight.
        * If that is false, we don't need to compute agreementGraphMaxCliqueWeight
        * as we know the value is going to be below 0 and thus useless for finalization.
        */
      def normalizedFaultTolerance(
          blockDag: BlockDagRepresentation[F],
          candidateBlockHash: BlockHash
      ): F[Float] =
        for {
          totalWeight <- computeTotalWeight(blockDag, candidateBlockHash)
          agreeingValidatorToWeight <- computeAgreeingValidatorToWeight(
                                        blockDag,
                                        candidateBlockHash
                                      )
          maxCliqueWeight <- if (2L * agreeingValidatorToWeight.values.sum < totalWeight) {
                              0L.pure[F]
                            } else {
                              agreementGraphMaxCliqueWeight(
                                blockDag,
                                candidateBlockHash,
                                agreeingValidatorToWeight
                              )
                            }
          faultTolerance = 2 * maxCliqueWeight - totalWeight
        } yield faultTolerance.toFloat / totalWeight

      private def computeTotalWeight(
          blockDag: BlockDagRepresentation[F],
          candidateBlockHash: BlockHash
      ): F[Long] =
        computeMainParentWeightMap(blockDag, candidateBlockHash).map(weightMapTotal)

      private def computeAgreeingValidatorToWeight(
          blockDag: BlockDagRepresentation[F],
          candidateBlockHash: BlockHash
      ): F[Map[Validator, Long]] =
        for {
          weights <- computeMainParentWeightMap(blockDag, candidateBlockHash)
          agreeingWeights <- weights.toList.traverse {
                              case (validator, stake) =>
                                for {
                                  maybeLatestMessageHash <- blockDag.latestMessageHash(validator)
                                  result <- maybeLatestMessageHash match {
                                             case Some(latestMessageHash) =>
                                               computeCompatibility(
                                                 blockDag,
                                                 candidateBlockHash,
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
                                } yield result
                            }
        } yield agreeingWeights.flatten.toMap

      private def computeMainParentWeightMap(
          blockDag: BlockDagRepresentation[F],
          candidateBlockHash: BlockHash
      ): F[Map[BlockHash, Long]] =
        blockDag.lookup(candidateBlockHash).flatMap { blockOpt =>
          blockOpt.get.parents.headOption match {
            case Some(parent) => blockDag.lookup(parent).map(_.get.weightMap)
            case None         => blockDag.lookup(candidateBlockHash).map(_.get.weightMap)
          }
        }

      private def agreementGraphMaxCliqueWeight(
          blockDag: BlockDagRepresentation[F],
          candidateBlockHash: BlockHash,
          agreeingValidatorToWeight: Map[Validator, Long]
      ): F[Long] = {
        def findAgreeingJustificationHash(
            justificationHashes: List[BlockHash],
            validator: Validator
        ): F[Option[BlockHash]] =
          justificationHashes.findM(justificationHash => {
            for {
              justificationOpt <- blockDag.lookup(justificationHash)
              result <- justificationOpt match {
                         case Some(justificationMetadata) =>
                           computeCompatibility(blockDag, candidateBlockHash, justificationHash)
                             .map { compatible =>
                               compatible && justificationMetadata.sender == validator
                             }
                         case None => false.pure[F]
                       }
            } yield result
          })

        def seesAgreement(
            first: Validator,
            second: Validator
        ): F[Boolean] =
          for {
            maybefirstLatestBlock <- blockDag.latestMessage(first)
            result <- maybefirstLatestBlock match {
                       case Some(firstLatestBlock) =>
                         val justificationHashes =
                           firstLatestBlock.justifications.map(_.latestBlockHash)
                         findAgreeingJustificationHash(
                           justificationHashes,
                           second
                         ).map(_.isDefined)
                       case None => false.pure[F]
                     }
          } yield result

        def filterChildren(
            candidate: BlockMetadata,
            validator: Validator
        ): F[List[BlockHash]] =
          for {
            latestMessageByValidatorHashOpt <- blockDag.latestMessageHash(validator)
            result <- latestMessageByValidatorHashOpt match {
                       case Some(latestMessageByValidatorHash) =>
                         DagOperations
                           .bfTraverseF[F, BlockHash](List(latestMessageByValidatorHash)) {
                             blockHash =>
                               ProtoUtil.getCreatorJustificationAsListByInMemory(
                                 blockDag,
                                 blockHash,
                                 validator,
                                 b => b == candidate.blockHash
                               )
                           }
                           .filterF(potentialChild => {
                             for {
                               metadata              <- blockDag.lookup(potentialChild).map(_.get)
                               isFutureBlock         = candidate.seqNum <= metadata.seqNum
                               validatorCreatedChild = metadata.sender == validator
                             } yield validatorCreatedChild && isFutureBlock
                           })
                           .flatMap(_.toList)
                       case None => List.empty[BlockHash].pure[F]
                     }
          } yield result

        def neverEventuallySeeDisagreement(
            first: Validator,
            second: Validator
        ): F[Boolean] =
          for {
            maybeFirstLatest <- blockDag.latestMessage(first)
            result <- maybeFirstLatest match {
                       case Some(firstLatestBlock) =>
                         val justificationHashes =
                           firstLatestBlock.justifications.map(_.latestBlockHash)
                         for {
                           justificationBlockSecondList <- justificationHashes.flatTraverse {
                                                            justificationHash =>
                                                              blockDag
                                                                .lookup(justificationHash)
                                                                .map(
                                                                  _.filter(_.sender == second).toList
                                                                )
                                                          }
                           justificationBlockSecond = justificationBlockSecondList.head
                           _ = assert(
                             justificationBlockSecondList
                               .forall(b => b.blockHash == justificationBlockSecond.blockHash)
                           )
                           potentialDisagreements <- filterChildren(
                                                      justificationBlockSecond,
                                                      second
                                                    )
                           result <- potentialDisagreements.forallM { potentialDisagreement =>
                                      computeCompatibility(
                                        blockDag,
                                        candidateBlockHash,
                                        potentialDisagreement
                                      )
                                    }
                         } yield result
                       case None => false.pure[F]
                     }
          } yield result

        def computeAgreementGraphEdges: F[List[(Validator, Validator)]] =
          (for {
            x <- agreeingValidatorToWeight.keys
            y <- agreeingValidatorToWeight.keys
            if x.toString > y.toString // TODO: Order ByteString
          } yield (x, y)).toList.filterA {
            case (first: Validator, second: Validator) =>
              seesAgreement(first, second) &&^ seesAgreement(second, first) &&^ neverEventuallySeeDisagreement(
                first,
                second
              ) &&^ neverEventuallySeeDisagreement(second, first)
          }

        computeAgreementGraphEdges.map { edges =>
          Clique.findMaximumCliqueByWeight[Validator](edges, agreeingValidatorToWeight)
        }
      }

      private def computeCompatibility(
          blockDag: BlockDagRepresentation[F],
          candidateBlockHash: BlockHash,
          targetBlockHash: BlockHash
      ): F[Boolean] =
        isInMainChain(blockDag, candidateBlockHash, targetBlockHash)
    }
}
