package coop.rchain.v2.casper.syntax

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.v2.casper.DependencyGraph.{CommonMessage, NoCommonMessage}
import coop.rchain.v2.casper.{DependencyGraph, SafetyOracle}
import fs2.{Chunk, Stream}

trait DependencyGraphSyntax {
  implicit final def DependencyGraphSyntax[F[_], M, S](
      c: DependencyGraph[F, M, S]
  ): DependencyGraphOps[F, M, S] =
    new DependencyGraphOps[F, M, S](c)
}

final class DependencyGraphOps[F[_], M, S](val dg: DependencyGraph[F, M, S]) extends AnyVal {
  import dg._

  def messageView(message: M)(implicit a: Sync[F], ordering: Ordering[M]): Stream[F, List[M]] =
    DependencyGraph.messageView(message, dg)

  def messagesView(
      messages: Set[M]
  )(implicit sync: Sync[F], ordering: Ordering[M]): Stream[F, Chunk[(M, List[M])]] =
    DependencyGraph.messagesView(messages, dg)

  def faultTolerances(
      agreeingMessages: Set[M],
      safetyOracle: SafetyOracle[F, M, S]
  )(implicit sync: Sync[F], ordering: Ordering[M]): Stream[F, List[(M, Float)]] =
    SafetyOracle.faultTolerances(agreeingMessages, safetyOracle, dg)

  def highestCommonMessage(
      messages: Set[M]
  )(implicit sync: Sync[F], ordering: Ordering[M]): F[Option[CommonMessage[M]]] =
    DependencyGraph.highestCommonMessage(messages, dg)

  /**
   * Stream of self justifications.
   */
  def selfJustificationChain(message: M)(implicit a: Sync[F]): Stream[F, M] = {
    val s = sender(message)
    Stream.unfoldEval(message)(justifications(_).map(_.find(sender(_) == s).map(v => (v, v))))
  }

  /**
   * Scope required to merge multiple messages.
   */
  def mergeScope(messages: Set[M])(implicit
      sync: Sync[F],
      ordering: Ordering[M]
  ): F[(CommonMessage[M], Set[M])] =
    highestCommonMessage(messages)
      .flatMap(_.liftTo[F](NoCommonMessage))
      .flatMap { case hcm @ CommonMessage(base, _, unseenPart) =>
        justifications(base)
          .flatMap { baseJustifications =>
            val streams = unseenPart.toList.map { fringeMessage =>
              Stream(fringeMessage) ++ selfJustificationChain(fringeMessage)
                .takeWhile(m => !(m == base || baseJustifications.contains(m)))
            }
            Stream
              .emits(streams)
              .covary[F]
              .flatten
              .compile
              .to(Set)
              .map(v => (hcm, v))
          }
      }

}
