package coop.rchain.casper.v2.core.syntax

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.v2.core.DependencyGraph.{CommonMessage, NoCommonMessage}
import coop.rchain.casper.v2.core._
import coop.rchain.shared.syntax.zipStreamList
import fs2.{Chunk, Stream}

trait DependencyGraphSyntax {
  implicit final def DependencyGraphSyntax[F[_], M, S](
      c: DependencyGraph[F, M, S]
  ): DependencyGraphOps[F, M, S] =
    new DependencyGraphOps[F, M, S](c)
}

final class DependencyGraphOps[F[_], M, S](val c: DependencyGraph[F, M, S]) extends AnyVal {
  import c._

  /**
    * @return Stream containing message + ancestors of the message (justifications), topologically sorted.
    *         Messages of the same topological order are sorted by identity.
    *         Can contain duplicates, as the same message can be seen through multiple paths.
    *         Not flattened to keep the notion of distance.
    */
  def toposortView(
      message: M
  )(relation: M => F[List[M]])(implicit a: Sync[F], ordering: Ordering[M]): Stream[F, List[M]] =
    Stream
      .unfoldLoopEval(List(message)) { lvl =>
        lvl
          .flatTraverse(s => relation(s))
          // Sort output to keep function pure
          .map(_.distinct.sorted)
          .map(next => (lvl, next.nonEmpty.guard[Option].as(next)))
      }

  /**
    * @return Stream containing message + ancestors of the message (justifications), topologically sorted.
    *         Messages of the same topological order are sorted by identity.
    *         Can contain duplicates, as the same message can be seen through multiple paths.
    *         Not flattened to keep the notion of distance.
    */
  def messageView(
      message: M
  )(relation: M => F[List[M]])(implicit a: Sync[F], ordering: Ordering[M]): Stream[F, List[M]] =
    toposortView(message)(relation)

  /**
    * @return Combined stream of views of the messages.
    *         Per message streams are zipped, to preserve topological sorting.
    */
  def messagesView(
      messages: Set[M]
  )(
      relation: M => F[List[M]]
  )(implicit sync: Sync[F], ordering: Ordering[M]): Stream[F, Chunk[(M, List[M])]] = {
    // Sort output to keep function pure
    val sorted = messages.toList.sorted
    zipStreamList(sorted.map(m => messageView(m)(relation).map((m, _)).zipWithIndex))
      .groupAdjacentBy { case (_, idx) => idx }
      .map { case (_, chunk) => chunk.map { case (v, _) => v } }
  }

  /**
    * @return Stream of self justifications.
    */
  def selfJustificationChain(message: M)(implicit a: Sync[F]): Stream[F, M] = {
    val s = sender(message)
    Stream.unfoldEval(message)(justifications(_).map(_.find(sender(_) == s).map(v => (v, v))))
  }

  def highestCommonMessage(
      messages: Set[M],
      requirement: M => Boolean = _ => true
  )(implicit sync: Sync[F], ordering: Ordering[M]): F[Option[CommonMessage[M]]] = {
    val latestSeqNums =
      messages.map(m => sender(m) -> seqNum(m)).toMap
    val initConnectionsMap = Map.empty[M, Set[M]]
    messagesView(messages)(c.parents)
      .flatMap(chunk => Stream.emits(chunk.toList.flatMap { case (s, t) => t.map((s, _)) }))
      .evalMapAccumulate(initConnectionsMap) {
        case (acc, (visitor, target)) =>
          val newVisitors = acc.getOrElse(target, Set.empty[M]) + visitor
          val newAcc      = acc.updated(target, newVisitors)
          // Messages that did not reach target through justifications yet
          val yetNotVisitors = messages -- newVisitors
          // If target has yetNotVisitors in justification - result is reached
          justifications(target)
            .map { js =>
              ((yetNotVisitors -- js).isEmpty && requirement(target))
                .guard[Option]
                .as(CommonMessage(target, js.toSet, newVisitors - target))
            }
            .map(v => (newAcc, v))
      }
      .map { case (_, v) => v }
      .zipWithIndex
      .mapAccumulate(List.empty[CommonMessage[M]]) {
        case (acc, (v, idx)) => (v.map(_ +: acc).getOrElse(acc), idx)
      }
      .mapFilter {
        case (acc, idx) =>
          acc.find {
            case CommonMessage(m, _, _) =>
              idx >= latestSeqNums(sender(m)) - seqNum(m)
          }
      }
      .take(1)
      .compile
      .last
  }

  /**
    * @return Scope required to merge multiple messages.
    */
  def mergeScope(messages: Set[M], requirement: M => Boolean = _ => true)(
      implicit sync: Sync[F],
      ordering: Ordering[M]
  ): F[(CommonMessage[M], Set[M])] =
    highestCommonMessage(messages, requirement)
      .flatMap(_.liftTo[F](NoCommonMessage))
      .flatMap {
        case hcm @ CommonMessage(base, _, unseenPart) =>
          justifications(base)
            .flatMap { baseJustifications =>
              val streams = unseenPart.toList.map { fringeMessage =>
                selfJustificationChain(fringeMessage)
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
