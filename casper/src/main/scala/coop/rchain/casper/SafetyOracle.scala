package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.{BlockMessage, Bond, Justification}
import coop.rchain.casper.util.ProtoUtil._
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

  /**
    * @param estimate Block to detect safety on
    * @return normalizedFaultTolerance float between -1 and 1, where -1 means potentially orphaned
    */
  def normalizedFaultTolerance(estimate: BlockMessage): F[Float]
}

object SafetyOracle {
  def apply[F[_]](implicit ev: SafetyOracle[F]): SafetyOracle[F] = ev
}

class TuranOracle(blocks: collection.Map[BlockHash, BlockMessage],
                  latestMessages: collection.Map[Validator, BlockMessage])
    extends SafetyOracle[Task] {
  def normalizedFaultTolerance(estimate: BlockMessage): Task[Float] = Task.delay {
    val faultTolerance = 2 * minMaxCliqueWeight(estimate) - totalWeight(estimate)
    faultTolerance.toFloat / totalWeight(estimate)
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

  private def candidateWeights(estimate: BlockMessage): Map[Validator, Int] = {
    val weights: Map[Validator, Int] = mainParentWeightMap(estimate)
    for {
      (validator, stake) <- weights
      latestMessage      <- latestMessages.get(validator) if compatible(estimate, latestMessage)
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
                                      candidates: Map[Validator, Int]): Int = {
    def seesAgreement(first: Validator, second: Validator): Boolean =
      (for {
        firstLatest <- latestMessages.get(first).toList
        justification <- firstLatest.justifications.map {
                          case Justification(_, latestBlock: BlockHash) => latestBlock
                        }
        justificationBlock <- blocks.get(justification)
        if justificationBlock.sender == second && compatible(estimate, justificationBlock)
      } yield justificationBlock).nonEmpty

    // TODO: Potentially replace with isInBlockDAG
    def filterChildren(candidate: BlockMessage,
                       blocks: collection.Map[BlockHash, BlockMessage]): List[BlockMessage] =
      blocks.values.filter { potentialChild =>
        isInMainChain(blocks, candidate, potentialChild)
      }.toList

    def neverEventuallySeeDisagreement(first: Validator, second: Validator): Boolean = {
      val potentialDisagreements: List[BlockMessage] =
        for {
          firstLatest <- latestMessages.get(first).toList
          justification <- firstLatest.justifications.map {
                            case Justification(_, latestBlock: BlockHash) => latestBlock
                          }
          justificationBlock <- blocks.get(justification).toList
          child              <- filterChildren(justificationBlock, blocks)
          if child.sender == second
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
      case (first: Validator, second: Validator) =>
        seesAgreement(first, second) && seesAgreement(second, first) &&
          neverEventuallySeeDisagreement(first, second) && neverEventuallySeeDisagreement(second,
                                                                                          first)
    }
    edges.size
  }

  // TODO: Change to isInBlockDAG
  private def compatible(candidate: BlockMessage, target: BlockMessage) =
    isInMainChain(blocks, candidate, target)

  // See Turan's theorem (https://en.wikipedia.org/wiki/Tur%C3%A1n%27s_theorem)
  private def maxCliqueMinSize(vertices: Int, edges: Int) = {
    val verticesSquared = vertices * vertices
    math.ceil(verticesSquared.toDouble / (verticesSquared - 2 * edges).toDouble).toInt
  }
}
