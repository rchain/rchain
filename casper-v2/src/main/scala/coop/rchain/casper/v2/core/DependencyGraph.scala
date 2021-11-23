package coop.rchain.casper.v2.core

trait DependencyGraph[F[_], M, S] {

  /**
    * Each message is justified by set of other messages.
    * List used here to omit conversion as this set is supposed to be traversed.
    */
  def justifications(message: M): F[List[M]]

  def parents(message: M): F[List[M]]

  def children(message: M): F[List[M]]

  /** Sender of a message. */
  def sender(message: M): S

  /** Sequence number of a message across sender's messages. */
  def seqNum(message: M): Int
}

object DependencyGraph {

  type LatestMessages[S, M] = List[(S, Set[M])]

  /**
    * Given set of messages S, the message that either ancestor or descendant for all messages in S.
    * The following picture shows common message for the set of messages (1,2,3,4)
    *
    *    1   2   3
    *      \  \  |
    *        \ \ |
    *          CM
    *             \
    *              \
    *               4
    *
    * @param message - result, the actual message. Can possibly be part of S.
    * @param ancestors - subset of S containing messages which are ancestors of the base
    * @param descendants - subset of S containing messages which are descendants of the base
    */
  final case class CommonMessage[M](message: M, ancestors: Set[M], descendants: Set[M]) {
    require((ancestors intersect descendants).isEmpty, "Wrong highest common message.")
  }

  val noCommonMsg = "Unable to find common message."
  case object NoCommonMessage extends Exception(noCommonMsg)
}
