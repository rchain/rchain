package coop.rchain.casper.v2.core
import cats.Monad
import cats.syntax.all._

trait DependencyGraph[F[_], M, S] {

  /**
    * Each message is justified by set of other messages.
    * List used here to omit conversion as this set is supposed to be traversed.
    */
  def justifications(message: M): F[List[M]]

  /**
    * Parents are subset o justifications through which all other justifications are visible.
    * This is optimisation for graph traversal to visit one justification only once.
    * Otherwise overhead on traversal might be significant.
    * This is similar to parents, but without ranking and sorting.
    * As parents should be stored to not compute each time, this is on a trait.
    */
  def parents(message: M): F[List[M]]

  /** Sender of a message. */
  def sender(message: M): S

  /** Sequence number of a message across sender's messages. */
  def seqNum(message: M): Int
}

object DependencyGraph {

  /**
    * Derive parents from justification.
    * */
  def computeParents[F[_]: Monad, M](
      targetJustifications: List[M],
      justifications: M => F[List[M]]
  ): F[List[M]] =
    targetJustifications
      .foldLeftM(targetJustifications.toSet) {
        case (acc, j) => justifications(j).map(jsLvl2 => acc -- jsLvl2)
      }
      .map(_.toList)

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
    require((ancestors intersect descendants).isEmpty, "Wrong common message.")
  }

  val noCommonMsg = "Unable to find common message."
  case object NoCommonMessage extends Exception(noCommonMsg)
}
