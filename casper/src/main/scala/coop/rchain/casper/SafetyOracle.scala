package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, Bond, Justification}
import util._
import monix.eval.Task

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
  def isSafe(estimate: BlockMessage, faultToleranceThreshold: Float): F[Boolean]
}

object SafetyOracle {
  def apply[F[_]](implicit ev: SafetyOracle[F]): SafetyOracle[F] = ev
}

class TuranOracle(blocks: collection.Map[ByteString, BlockMessage],
                  latestBlocks: collection.Map[ByteString, BlockMessage])
    extends SafetyOracle[Task] {
  def isSafe(estimate: BlockMessage, faultToleranceThreshold: Float): Task[Boolean] = Task.delay {
    val faultTolerance = 2 * minMaxCliqueWeight(estimate) - totalWeight(estimate)
    faultTolerance >= faultToleranceThreshold * totalWeight(estimate)
  }

  private def minMaxCliqueWeight(estimate: BlockMessage): Int =
    // To have a maximum clique of half the total weight,
    // you need at least twice the weight of the candidateWeights to be greater than the total weight
    if (2 * candidateWeights(estimate).values.sum < totalWeight(estimate)) {
      0
    } else {
      val vertexCount = candidateWeights(estimate).keys.size
      val edgeCount   = agreementGraphEdgeCount(estimate, candidateWeights(estimate))
      minTotalValidatorWeight(estimate, maxCliqueMinSize(vertexCount, edgeCount))
    }

  private def totalWeight(estimate: BlockMessage): Int =
    weightMapTotal(mainParentWeightMap(estimate))

  private def candidateWeights(estimate: BlockMessage): Map[ByteString, Int] = {
    val weights: Map[ByteString, Int] = mainParentWeightMap(estimate)
    for {
      (validator, stake) <- weights
      latestBlock        <- latestBlocks.get(validator) if compatible(estimate, latestBlock)
    } yield (validator, stake)
  }

  private def mainParentWeightMap(estimate: BlockMessage) = {
    val estimateMainParent = mainParent(blocks, estimate)
    estimateMainParent match {
      case Some(parent) => weightMap(parent)
      case None         => weightMap(estimate) // Genesis
    }
  }

  private def agreementGraphEdgeCount(estimate: BlockMessage,
                                      candidates: Map[ByteString, Int]): Int = {
    def seesAgreement(first: ByteString, second: ByteString): Boolean =
      (for {
        firstLatest <- latestBlocks.get(first).toList
        justification <- firstLatest.justifications.map {
                          case Justification(_, latestBlock: ByteString) => latestBlock
                        }
        justificationBlock <- blocks.get(justification)
        if justificationBlock.sig == second && compatible(estimate, justificationBlock)
      } yield justificationBlock).nonEmpty

    // TODO: Potentially replace with isInBlockDAG
    def filterChildren(candidate: BlockMessage,
                       blocks: collection.Map[ByteString, BlockMessage]): List[BlockMessage] =
      blocks.values.filter { potentialChild =>
        isInMainChain(blocks, candidate, potentialChild)
      }.toList

    def neverEventuallySeeDisagreement(first: ByteString, second: ByteString): Boolean = {
      val potentialDisagreements: List[BlockMessage] =
        for {
          firstLatest <- latestBlocks.get(first).toList
          justification <- firstLatest.justifications.map {
                            case Justification(_, latestBlock: ByteString) => latestBlock
                          }
          justificationBlock <- blocks.get(justification).toList
          child              <- filterChildren(justificationBlock, blocks)
          if child.sig == second
        } yield child
      potentialDisagreements.forall { potentialDisagreement =>
        compatible(estimate, potentialDisagreement)
      }
    }

    val edges = (for {
      x <- candidates.keys
      y <- candidates.keys
      if x.toString > y.toString // TODO: Order ByteString
    } yield (x, y)) filter {
      case (validatorOne: ByteString, validatorTwo: ByteString) =>
        seesAgreement(validatorOne, validatorTwo) && seesAgreement(validatorTwo, validatorOne) &&
          neverEventuallySeeDisagreement(validatorOne, validatorTwo) && neverEventuallySeeDisagreement(
          validatorTwo,
          validatorOne)
    }
    edges.size
  }

  // TODO: Change to isInBlockDAG
  private def compatible(candidate: BlockMessage, target: BlockMessage) =
    isInMainChain(blocks, candidate, target)

  // See Turan's theorem (https://en.wikipedia.org/wiki/Tur%C3%A1n%27s_theorem)
  private def maxCliqueMinSize(vertices: Int, edges: Int) = {
    val verticesSquared = vertices * vertices
    math.ceil(verticesSquared.toFloat / (verticesSquared - 2 * edges).toFloat).toInt
  }
}
