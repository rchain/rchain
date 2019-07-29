package coop.rchain.rspace

import cats.Id
import coop.rchain.rspace.internal._

import scala.collection.SortedSet

final case class Result[A](value: A, persistent: Boolean)
final case class ContResult[C, P, A](
    value: A,
    persistent: Boolean,
    channels: Seq[C],
    patterns: Seq[P],
    sequenceNumber: Int,
    peek: Boolean = false
)

/** The interface for RSpace
  *
  * @tparam C a type representing a channel
  * @tparam P a type representing a pattern
  * @tparam A a type representing an arbitrary piece of data and match result
  * @tparam K a type representing a continuation
  */
trait ISpace[F[_], C, P, A, K] {

  /** Searches the store for data matching all the given patterns at the given channels.
    *
    * If no match is found, then the continuation and patterns are put in the store at the given
    * channels.
    *
    * If a match is found, then the continuation is returned along with the matching data.
    *
    * Matching data stored with the `persist` flag set to `true` will not be removed when it is
    * retrieved. See below for more information about using the `persist` flag.
    *
    * '''NOTE''':
    *
    * A call to [[consume]] that is made with the persist flag set to `true` only persists when
    * there is no matching data.
    *
    * This means that in order to make a continuation "stick" in the store, the user will have to
    * continue to call [[consume]] until a `None` is received.
    *
    * @param channels A Seq of channels on which to search for matching data
    * @param patterns A Seq of patterns with which to search for matching data
    * @param continuation A continuation
    * @param persist Whether or not to attempt to persist the data
    */
  def consume(
      channels: Seq[C],
      patterns: Seq[P],
      continuation: K,
      persist: Boolean,
      sequenceNumber: Int = 0,
      peeks: SortedSet[Int] = SortedSet.empty
  )(
      implicit m: Match[F, P, A]
  ): F[Option[(ContResult[C, P, K], Seq[Result[A]])]]

  def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[F, P, A]
  ): F[Option[(K, Seq[A])]]

  /** Searches the store for a continuation that has patterns that match the given data at the
    * given channel.
    *
    * If no match is found, then the data is put in the store at the given channel.
    *
    * If a match is found, then the continuation is returned along with the matching data.
    *
    * Matching data or continuations stored with the `persist` flag set to `true` will not be
    * removed when they are retrieved. See below for more information about using the `persist`
    * flag.
    *
    * '''NOTE''':
    *
    * A call to [[produce]] that is made with the persist flag set to `true` only persists when
    * there are no matching continuations.
    *
    * This means that in order to make a piece of data "stick" in the store, the user will have to
    * continue to call [[produce]] until a `None` is received.
    *
    * @param channel A channel on which to search for matching continuations and/or store data
    * @param data A piece of data
    * @param persist Whether or not to attempt to persist the data
    */
  def produce(channel: C, data: A, persist: Boolean, sequenceNumber: Int = 0)(
      implicit m: Match[F, P, A]
  ): F[Option[(ContResult[C, P, K], Seq[Result[A]])]]

  /** Creates a checkpoint.
    *
    * @return A [[Checkpoint]]
    */
  def createCheckpoint(): F[Checkpoint]

  /** Resets the store to the given root.
    *
    * @param root A BLAKE2b256 Hash representing the checkpoint
    */
  def reset(root: Blake2b256Hash): F[Unit]

  def getData(channel: C): F[Seq[Datum[A]]]

  def getWaitingContinuations(channels: Seq[C]): F[Seq[WaitingContinuation[P, K]]]

  /** Clears the store.  Does not affect the history trie.
    */
  def clear(): F[Unit]

  /** Closes the ISpace freeing all underlying resources.
    */
  def close(): F[Unit]

  // TODO: this should not be exposed
  def toMap: F[Map[Seq[C], Row[P, A, K]]]

  /**
    Allows to create a "soft" checkpoint which doesn't persist the checkpointed data into history.
    This operation is significantly faster than {@link #createCheckpoint()} because the computationally
    expensive operation of creating the history trie is avoided.
    */
  def createSoftCheckpoint(): F[SoftCheckpoint[C, P, A, K]]

  /**
    Reverts the ISpace to the state checkpointed using {@link #createSoftCheckpoint()}
    */
  def revertToSoftCheckpoint(checkpoint: SoftCheckpoint[C, P, A, K]): F[Unit]
}

//TODO lookinto to removing  ISpace object.
object ISpace {
  type IdISpace[C, P, A, K] = ISpace[Id, C, P, A, K]
}
