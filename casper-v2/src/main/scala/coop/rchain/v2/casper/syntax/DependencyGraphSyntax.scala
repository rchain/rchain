package coop.rchain.v2.casper.syntax

import cats.syntax.all._
import coop.rchain.v2.casper.DependencyGraph.{CommonMessage, NoCommonMessage}
import coop.rchain.v2.casper.{DependencyGraph, SafetyOracle}
import fs2.{Chunk, Pure, Stream}

trait DependencyGraphSyntax {
  implicit final def DependencyGraphSyntax[F[_], M, S](
      c: DependencyGraph[M, S]
  ): DependencyGraphOps[M, S] =
    new DependencyGraphOps[M, S](c)
}

final class DependencyGraphOps[M, S](val dg: DependencyGraph[M, S]) extends AnyVal {
  import dg._

  def messageView(message: M)(implicit ordering: Ordering[M]): Stream[Pure, List[M]] =
    DependencyGraph.messageView(message, dg)

  def messagesView(
      messages: Set[M]
  )(implicit ordering: Ordering[M]): Stream[Pure, Chunk[(M, List[M])]] =
    DependencyGraph.messagesView(messages, dg)

  def faultTolerances(
      agreeingMessages: Set[M],
      safetyOracle: SafetyOracle[M, S]
  )(implicit ordering: Ordering[M]): Stream[Pure, List[(M, Float)]] =
    SafetyOracle.faultTolerances(agreeingMessages, safetyOracle, dg)

  def mergeScope(
      messages: Set[M]
  )(implicit ordering: Ordering[M]): Option[(CommonMessage[M], Set[M])] =
    DependencyGraph.mergeScope(messages, dg)

  def highestCommonMessage(
      messages: Set[M]
  )(implicit ordering: Ordering[M]): Option[CommonMessage[M]] =
    DependencyGraph.highestCommonMessage(messages, dg)

  /**
   * Stream of self justifications.
   */
  def selfJustificationChain(message: M): Stream[Pure, M] = {
    val s = sender(message)
    Stream.unfold(message)(justifications(_).find(sender(_) == s).map(v => (v, v)))
  }

}
