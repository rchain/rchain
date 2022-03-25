package coop.rchain.sdk.block.state

import coop.rchain.sdk.block.state.Buffered.{AckValidatedEffect, BufferedEffect, BufferedState}
import cats.syntax.all._

import scala.collection.immutable.Set

/** State of DAG buffer - blocks that are received but not validated.. */
final case class Buffered[BId](st: BufferedState[BId]) {
  type ST = Buffered[BId]

  def add(
      bid: BId,
      deps: Set[BId]
  )(requested: Set[BId], validated: Set[BId]): (ST, BufferedEffect[BId]) = {
    val newSt = {
      val newChildMap = deps.foldLeft(st.childMap.updated(bid, Set())) {
        case (acc, d) => acc.updated(d, acc.get(d).map(_ + bid).getOrElse(Set()))
      }
      st.copy(childMap = newChildMap)
    }
    val toRequest = deps.filterNot(bid => validated.contains(bid) || requested.contains(bid))
    val effect    = BufferedEffect(toRequest, deps.forall(validated.contains))
    (Buffered(newSt), effect)
  }

  def ackValidated(bid: BId): (ST, AckValidatedEffect[BId]) = {
    val (newSt, unlocked) = {
      val newChildMap = st.childMap - bid
      assert(st.childMap.contains(bid), "Child map does not have id on ackValidated.")
      val (unlocked, newDepsMap) = st.depsMap.iterator
        .map { case (b, deps) => (b, deps - bid) }
        .partition { case (_, deps) => deps.isEmpty }
        .bimap(_.map(_._1).toSet, _.toMap)
      val newDepsFree = st.depsFree ++ unlocked
      (BufferedState(newChildMap, newDepsMap, newDepsFree), unlocked)
    }
    val effect = AckValidatedEffect(unlocked)
    (Buffered(newSt), effect)
  }

  def contains(bid: BId): Boolean  = st.childMap.contains(bid)
  def readyForValidation: Set[BId] = st.depsFree
}

object Buffered {
  // State definition
  final case class BufferedState[BId](
      childMap: Map[BId, Set[BId]],
      depsMap: Map[BId, Set[BId]],
      depsFree: Set[BId]
  )

  // Effects
  final case class BufferedEffect[BId](depsToRequest: Set[BId], readyForValidation: Boolean)
  final case class AckValidatedEffect[BId](unlocked: Set[BId])
}
