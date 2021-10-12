package coop.rchain.v2.casper
import cats.Monad
import cats.effect.Sync
import coop.rchain.v2.casper.syntax.all._
import cats.syntax.all._
import coop.rchain.shared.syntax.zipStreamList
import fs2.{Chunk, Stream}

/**
 * Casper dependency graph.
 */
trait DependencyGraph[F[_], M, S] {

  /**
   * Each message is justified by set of other messages.
   * List used here to omit conversion as this set is supposed to be traversed.
   */
  def justifications(message: M): F[List[M]]

  /**
   * Parents are subset of justifications through which all other justifications are visible.
   * This is optimisation for graph traversal to visit a justification only once.
   * Otherwise overhead on traversal might be significant.
   * This is similar to parents, but without ranking and sorting.
   * As parents should be stored to not compute each time, this is on a trait.
   */
  def parents(message: M): F[List[M]]

  /**
   * Sender of a message.
   */
  def sender(message: M): S

  /**
   * Sequence number of a message.
   */
  def seqNum(message: M): Long
}

object DependencyGraph {

  /**
   * Derive parents from justification.
   */
  def computeParents[F[_]: Monad, M](
      targetJustifications: List[M],
      justifications: M => F[List[M]]
  ): F[List[M]] =
    targetJustifications
      .foldLeftM(targetJustifications.toSet) { case (acc, j) =>
        justifications(j).map(jsLvl2 => acc -- jsLvl2)
      }
      .map(_.toList)

  /**
   * Stream containing message + ancestors of the message (parents), topologically sorted.
   * Messages of the same topological order are sorted by identity.
   * Not flattened to keep the notion of distance.
   */
  def messageView[F[_], M, S](
      message: M,
      dg: DependencyGraph[F, M, S]
  )(implicit a: Sync[F], ordering: Ordering[M]): Stream[F, List[M]] =
    Stream
      .unfoldLoopEval(List(message)) { lvl =>
        lvl
          .flatTraverse(s => dg.parents(s))
          // Sort output to keep function pure
          .map(_.distinct.sorted)
          .map(next => (lvl, next.nonEmpty.guard[Option].as(next)))
      }

  /**
   * Combined stream of views of the messages.
   * Per message streams are zipped, to preserve topological sorting.
   */
  def messagesView[F[_], M, S](
      messages: Set[M],
      dg: DependencyGraph[F, M, S]
  )(implicit sync: Sync[F], ordering: Ordering[M]): Stream[F, Chunk[(M, List[M])]] = {
    // Sort output to keep function pure
    val sorted = messages.toList.sorted
    zipStreamList(sorted.map(m => messageView(m, dg).map((m, _)).zipWithIndex))
      .groupAdjacentBy { case (_, idx) => idx }
      .map { case (_, chunk) => chunk.map { case (v, _) => v } }
  }

  /**
   * Given set of messages S, the message that either ancestor or descendant for all messages in S.
   * The following picture shows common message for the set of messages (1,2,3,4).
   *
   *    1   2   3
   *      \  \  |
   *        \ \ |
   *          CM
   *             \
   *              \
   *               4
   *
   * @param message      Result, the actual message. Can possibly be part of S.
   * @param ancestors    Subset of S which is ancestors of the base
   * @param descendants  Subset of S which is descendants of the base
   */
  final case class CommonMessage[M](message: M, ancestors: Set[M], descendants: Set[M]) {
    require((ancestors intersect descendants).isEmpty, "Wrong common message.")
  }

  case object NoCommonMessage extends Exception("Unable to find common message.")

  /**
   * Find the highest message common to @targets according to @dg.
   */
  def highestCommonMessage[F[_], M, S](
      targets: Set[M],
      dg: DependencyGraph[F, M, S]
  )(implicit sync: Sync[F], ordering: Ordering[M]): F[Option[CommonMessage[M]]] = {
    import dg._
    val latestSeqNums      = targets.map(m => sender(m) -> seqNum(m)).toMap
    val initConnectionsMap = Map.empty[M, Set[M]]
    dg.messagesView(targets)
      .flatMap(chunk => Stream.emits(chunk.toList.flatMap { case (s, t) => t.map((s, _)) }))
      .evalMapAccumulate(initConnectionsMap) { case (acc, (visitor, target)) =>
        val newVisitors    = acc.getOrElse(target, Set.empty[M]) + visitor
        val newAcc         = acc.updated(target, newVisitors)
        // Messages that did not reach target through justifications yet
        val yetNotVisitors = targets -- newVisitors
        // If target has yetNotVisitors in justification - result is reached
        justifications(target)
          .map { js =>
            (yetNotVisitors -- js).isEmpty
              .guard[Option]
              .as(CommonMessage(target, js.toSet, newVisitors - target))
          }
          .map(v => (newAcc, v))
      }
      .map { case (_, v) => v }
      .zipWithIndex
      .mapAccumulate(List.empty[CommonMessage[M]]) { case (acc, (v, idx)) =>
        (v.map(_ +: acc).getOrElse(acc), idx)
      }
      .mapFilter { case (acc, idx) =>
        acc.find { case CommonMessage(m, _, _) =>
          idx >= latestSeqNums(sender(m)) - seqNum(m)
        }
      }
      .take(1)
      .compile
      .last
  }
}
