package coop.rchain.blockstorage.casper
import cats.Monad
import cats.syntax.all._
import fs2.Stream

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

  def computeParents[F[_]: Monad, M, S](m: M, justifications: M => F[List[M]]): F[List[M]] =
    for {
      js      <- justifications(m)
      jsLvl2  <- js.flatTraverse(justifications)
      parents = js diff jsLvl2.distinct
    } yield parents

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

  def zipStreamList[F[_], A](streams: List[Stream[F, A]]): Stream[F, A] =
    streams
      .foldLeft[Stream[F, List[Option[A]]]](Stream.empty) { (acc, s) =>
        zipStreams(acc, s)
      }
      .flatMap(l => Stream.emits(l.reverse.flatten))

  def zipStreams[F[_], A](
      s1: Stream[F, List[Option[A]]],
      s2: Stream[F, A]
  ): Stream[F, List[Option[A]]] =
    s1.zipAllWith(s2.map(Option(_)))(Nil, Option.empty[A]) { case (acc, a) => a :: acc }

}
