package coop.rchain.casper

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol.{BlockMessage, Bond, Justification}
import util._

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
 * If all validators are indeed part of the clique, min_max_clique_weight will hopefully be equal to total_weight
 * and is_safe will reduce to total_weight >= total_weight and evaluate to true.
 */
class TuranOracle(blocks: collection.Map[ByteString, BlockMessage],
                  estimate: BlockMessage,
                  latestBlocks: collection.Map[ByteString, BlockMessage],
                  fault_tolerance_threshold: Float) {
  def total_weight(): Int = weightMapTotal(estimate)
  def is_safe(): Boolean = {
    val fault_tolerance = 2 * min_max_clique_weight() - total_weight()
    fault_tolerance >= fault_tolerance_threshold * total_weight()
  }

  private def min_max_clique_weight(): Int =
    if (2 * candidates().values.sum < total_weight()) {
      0
    } else {
      val vertexCount = candidates().keys.size
      val edgeCount   = agreementGraphEdgeCount(blocks, estimate, latestBlocks, candidates())
      minTotalValidatorWeight(estimate, maxCliqueMinSize(vertexCount, edgeCount))
    }

  private def candidates(): Map[ByteString, Int] = {
    val estimateMainParent = mainParent(blocks, estimate)
    val weights = estimateMainParent match {
      case Some(parent) => weightMap(parent)
      case None         => weightMap(estimate) // Genesis
    }
    for {
      (validator, stake) <- weights
      latestBlock        <- latestBlocks.get(validator) if compatible(estimate, latestBlock)
    } yield (validator, stake)
  }

  // TODO: Add free messages
  def agreementGraphEdgeCount(blocks: collection.Map[ByteString, BlockMessage],
                              estimate: BlockMessage,
                              latestBlocks: collection.Map[ByteString, BlockMessage],
                              candidates: Map[ByteString, Int]): Int = {
    def seesAgreement(self: ByteString, other: ByteString): Boolean =
      (for {
        selfLatest <- latestBlocks.get(self).toList
        justification <- selfLatest.justifications
                          .map {
                            case Justification(creator: ByteString, latestBlock: ByteString) =>
                              creator -> latestBlock
                          }
                          .toMap
                          .values
                          .toList
        justificationBlock <- blocks.get(justification)
        if justificationBlock.sig == other
        if compatible(estimate, justificationBlock)
      } yield justificationBlock).size == 1

    val edges = (for {
      x <- candidates.keys
      y <- candidates.keys
      if x.toString > y.toString // TODO: Order ByteString
    } yield (x, y)) filter {
      case (validatorOne: ByteString, validatorTwo: ByteString) =>
        seesAgreement(validatorOne, validatorTwo) && seesAgreement(validatorTwo, validatorOne)
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
