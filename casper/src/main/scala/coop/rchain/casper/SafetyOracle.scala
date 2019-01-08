package coop.rchain.casper

import cats.Monad
import cats.implicits._
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockMetadata}
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.util.ProtoUtil._
import coop.rchain.casper.util.{Clique, DagOperations, ProtoUtil}
import coop.rchain.shared.Log

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
  def normalizedFaultTolerance(
      blockDag: BlockDagRepresentation[F],
      estimateBlockHash: BlockHash
  ): F[Float]
}

object SafetyOracle extends SafetyOracleInstances {
  def apply[F[_]](implicit ev: SafetyOracle[F]): SafetyOracle[F] = ev
}

sealed abstract class SafetyOracleInstances {
  def turanOracle[F[_]: Monad: Log]: SafetyOracle[F] =
    new SafetyOracle[F] {
      def normalizedFaultTolerance(
          blockDag: BlockDagRepresentation[F],
          estimateBlockHash: BlockHash
      ): F[Float] =
        for {
          totalWeight        <- computeTotalWeight(blockDag, estimateBlockHash)
          minMaxCliqueWeight <- computeMinMaxCliqueWeight(blockDag, estimateBlockHash)
          faultTolerance     = 2 * minMaxCliqueWeight - totalWeight
        } yield faultTolerance.toFloat / totalWeight

      // To have a maximum clique of half the total weight,
      // you need at least twice the weight of the candidateWeights to be greater than the total weight
      private def computeMinMaxCliqueWeight(
          blockDag: BlockDagRepresentation[F],
          estimateBlockHash: BlockHash
      ): F[Long] =
        for {
          candidateWeights <- computeCandidateWeights(blockDag, estimateBlockHash)
          totalWeight      <- computeTotalWeight(blockDag, estimateBlockHash)
          result <- if (2L * candidateWeights.values.sum < totalWeight) {
                     0L.pure[F]
                   } else {
                     val vertexCount = candidateWeights.keys.size
                     agreementGraphEdgeCount(blockDag, estimateBlockHash, candidateWeights)
                       .flatMap { edgeCount =>
                         minTotalValidatorWeight(
                           blockDag,
                           estimateBlockHash,
                           maxCliqueMinSize(vertexCount, edgeCount)
                         )
                       }
                   }
        } yield result

      private def computeTotalWeight(
          blockDag: BlockDagRepresentation[F],
          estimateBlockHash: BlockHash
      ): F[Long] =
        computeMainParentWeightMap(blockDag, estimateBlockHash).map(weightMapTotal)

      private def computeCandidateWeights(
          blockDag: BlockDagRepresentation[F],
          estimateBlockHash: BlockHash
      ): F[Map[Validator, Long]] =
        for {
          weights <- computeMainParentWeightMap(blockDag, estimateBlockHash)
          candidateWeights <- weights.toList.traverse {
                               case (validator, stake) =>
                                 for {
                                   maybeLatestMessageHash <- blockDag.latestMessageHash(validator)
                                   result <- maybeLatestMessageHash match {
                                              case Some(latestMessageHash) =>
                                                computeCompatibility(
                                                  blockDag,
                                                  estimateBlockHash,
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
        } yield candidateWeights.flatten.toMap

      private def computeMainParentWeightMap(
          blockDag: BlockDagRepresentation[F],
          estimateBlockHash: BlockHash
      ): F[Map[BlockHash, Long]] =
        blockDag.lookup(estimateBlockHash).flatMap { blockOpt =>
          blockOpt.get.parents.headOption match {
            case Some(parent) => blockDag.lookup(parent).map(_.get.weightMap)
            case None         => blockDag.lookup(estimateBlockHash).map(_.get.weightMap)
          }
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
          blockDag: BlockDagRepresentation[F],
          estimateBlockHash: BlockHash,
          candidates: Map[Validator, Long]
      ): F[Int] = {
        def findAgreeingJustificationHash(
            justificationHashes: List[BlockHash],
            validator: Validator
        ): F[Option[BlockHash]] =
          justificationHashes.findM(justificationHash => {
            for {
              justificationOpt <- blockDag.lookup(justificationHash)
              result <- justificationOpt match {
                         case Some(justificationMetadata) =>
                           computeCompatibility(blockDag, estimateBlockHash, justificationHash)
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
                           _ <- Monad[F].ifM((justificationBlockSecondList.length != 1).pure[F])(
                                 for {
                                   _ <- Log[F].info(
                                         "f: " + PrettyPrinter
                                           .buildString(first) + " -- s: " + PrettyPrinter
                                           .buildString(second)
                                       )
                                   _ <- Log[F].info(
                                         "firstLatestBlock: " + PrettyPrinter.buildString(
                                           firstLatestBlock.blockHash
                                         ) + " parents: " + firstLatestBlock.parents
                                           .map(PrettyPrinter.buildString)
                                           .mkString(";") + " justifications " + firstLatestBlock.justifications
                                           .map(
                                             j =>
                                               "V: " + PrettyPrinter
                                                 .buildString(j.validator) + " LBH " + PrettyPrinter
                                                 .buildString(j.latestBlockHash)
                                           )
                                           .mkString(";")
                                       )
                                   _ <- Log[F].info(
                                         "justifications 2nd list: " + justificationBlockSecondList
                                           .map(
                                             j =>
                                               PrettyPrinter
                                                 .buildString(j.blockHash) + " parents: " + j.parents
                                                 .map(PrettyPrinter.buildString)
                                                 .mkString(";") + " justifications " + j.justifications
                                                 .map(
                                                   _j =>
                                                     "V: " + PrettyPrinter
                                                       .buildString(_j.validator) + " LBH " + PrettyPrinter
                                                       .buildString(_j.latestBlockHash)
                                                 )
                                                 .mkString(";")
                                           )
                                           .mkString(" <<>> ")
                                       )
                                 } yield (),
                                 ().pure[F]
                               )
                           _                        = assert(justificationBlockSecondList.length == 1)
                           justificationBlockSecond = justificationBlockSecondList.head
                           potentialDisagreements <- filterChildren(
                                                      justificationBlockSecond,
                                                      second
                                                    )
                           result <- potentialDisagreements.forallM { potentialDisagreement =>
                                      computeCompatibility(
                                        blockDag,
                                        estimateBlockHash,
                                        potentialDisagreement
                                      )
                                    }
                         } yield result
                       case None => false.pure[F]
                     }
          } yield result

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
                    neverEventuallySeeDisagreement(second, first),
                    false.pure[F]
                  ),
                  false.pure[F]
                ),
                false.pure[F]
              )
          }
        computeAgreementGraphEdges.map { edges =>
          findMaximumClique(edges, candidates)._1.size
        }
      }

      // TODO: Change to isInBlockDAG
      private def computeCompatibility(
          blockDag: BlockDagRepresentation[F],
          candidateBlockHash: BlockHash,
          targetBlockHash: BlockHash
      ): F[Boolean] =
        isInMainChain(blockDag, candidateBlockHash, targetBlockHash)

      // See Turan's theorem (https://en.wikipedia.org/wiki/Tur%C3%A1n%27s_theorem)
      private def maxCliqueMinSize(vertices: Int, edges: Int) = {
        val verticesSquared = vertices * vertices
        math.ceil(verticesSquared.toDouble / (verticesSquared - 2 * edges).toDouble).toInt
      }
    }
}
