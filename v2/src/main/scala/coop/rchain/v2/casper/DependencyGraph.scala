package coop.rchain.v2.casper
import cats.Monad
import cats.syntax.all._
import coop.rchain.shared.syntax.zipStreamList
import coop.rchain.v2.casper.syntax.all._
import fs2.{Chunk, Pure, Stream}

/**
 * Casper dependency graph.
 */
trait DependencyGraph[M, S] {

  /**
   * Each message is justified by set of other messages.
   * List used here to omit conversion as this set is supposed to be traversed.
   */
  def justifications(message: M): Set[M]

  /**
   * Parents are subset of justifications through which all other justifications are visible.
   * This is optimisation for graph traversal to visit a justification only once.
   * Otherwise overhead on traversal might be significant.
   * This is similar to parents, but without ranking and sorting.
   * As parents should be stored to not compute each time, this is on a trait.
   */
  def parents(message: M): Set[M]

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
  def computeParents[M](
      targetJustifications: Set[M],
      justifications: M => Set[M]
  ): Set[M] =
    targetJustifications.foldLeft(targetJustifications) { case (acc, j) =>
      acc -- justifications(j)
    }

  /**
   * Stream containing message + ancestors of the message (parents), topologically sorted.
   * Messages of the same topological order are sorted by identity.
   * Not flattened to keep the notion of distance.
   */
  def messageView[M, S](
      message: M,
      dg: DependencyGraph[M, S]
  )(implicit ordering: Ordering[M]): Stream[Pure, List[M]] =
    Stream.unfoldLoop(List(message)) { lvl =>
      val next = lvl
        .flatMap(s => dg.parents(s))
        // Sort output to keep function pure
        .distinct
        .sorted
      (lvl, next.nonEmpty.guard[Option].as(next))
    }

  /**
   * Combined stream of views of the messages.
   * Per message streams are zipped, to preserve topological sorting.
   */
  def messagesView[M, S](
      messages: Set[M],
      dg: DependencyGraph[M, S]
  )(implicit ordering: Ordering[M]): Stream[Pure, Chunk[(M, List[M])]] = {
    // Sort output to keep function pure
    val sorted = messages.toList.sorted
    zipStreamList(sorted.map(m => messageView(m, dg).map((m, _)).zipWithIndex))
      .groupAdjacentBy { case (_, idx) => idx }
      .map { case (_, chunk) => chunk.map { case (v, _) => v } }
  }

  /**
   * Given set of messages S (target set), the message that either ancestor or descendant for all messages in S.
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
   * One of targets can me CM as well
   *
   *     1   2
   *      \  \
   *        \ \
   *            3 <- (CM)
   *             \
   *              \
   *               4
   *
   * @param message      Result, the actual message. Can possibly be part of S.
   * @param ancestors    Subset of S which is ancestors of the base
   * @param descendants  Subset of S which is descendants of the base
   */
  final case class CommonMessage[M](message: M, ancestors: Set[M], descendants: Set[M]) {
    require(
      (ancestors intersect descendants).isEmpty,
      "Wrong common message: ancestors intersect with descendant."
    )
  }

  /**
   * Find the highest message common to @targets according to @dg.
   */
  def highestCommonMessage[F[_], M, S](
      targets: Set[M],
      dg: DependencyGraph[M, S]
  )(implicit ordering: Ordering[M]): Option[CommonMessage[M]] = {
    import dg._
    val latestSeqNums      = targets.map(m => sender(m) -> seqNum(m)).toMap
    val initConnectionsMap = Map.empty[M, Set[M]]
    dg.messagesView(targets)
      .flatMap(chunk => Stream.emits(chunk.toList.flatMap { case (s, t) => t.map((s, _)) }))
      .mapAccumulate(initConnectionsMap) { case (acc, (visitor, target)) =>
        val newVisitors    = acc.getOrElse(target, Set.empty[M]) + visitor
        val newAcc         = acc.updated(target, newVisitors)
        // Messages that did not reach target through justifications yet
        val yetNotVisitors = targets -- newVisitors
        // If target has yetNotVisitors in justification - result is reached
        val js             = justifications(target)
        val v              = (yetNotVisitors -- js).isEmpty
          .guard[Option]
          .as(CommonMessage(target, js, newVisitors - target))
        (newAcc, v)
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

  /**
   * Scope required to merge set of messages (target set).
   *
   * To merge set of messages, the highest [[CommonMessage]] should be found and used as a base for merge.
   * The highest means that it should be closest to target set and provide minimal merge set.
   * All messages down from target set that are not seen by the base are the merge set.
   * NOTE:  Conflict resolution should be applied on merge set, it is not guaranteed to be free of conflicts..
   *        It is free from conflicts if target set is a [[coop.rchain.v2.casper.data.FinalizationFringe]] under
   *        finalized rejection conditions.
   *
   * @param commonMessage The highest [[CommonMessage]].
   * @param mergeSet      Set of messages in merge scope.
   * @tparam M            Type of the message.
   */
  final case class MergeScope[M](
      commonMessage: CommonMessage[M],
      mergeSet: Set[M]
  )

  /**
   * Find [[MergeScope]] for set of messages.
   */
  def findMergeScope[M, S](
      targetSet: Set[M],
      dg: DependencyGraph[M, S]
  )(implicit ordering: Ordering[M]): Option[MergeScope[M]] =
    highestCommonMessage(targetSet, dg)
      .map { case hcm @ CommonMessage(base, _, unseenPart) =>
        val perSenderMsgStreams = unseenPart.toList.map { fringeMessage =>
          Stream(fringeMessage) ++
            dg.selfJustificationChain(fringeMessage)
              .takeWhile(m => !(m == base || dg.justifications(base).contains(m)))
        }
        val mergeSet            = Stream.emits(perSenderMsgStreams).flatten.compile.to(Set)
        MergeScope(hcm, mergeSet)
      }
}
