package coop.rchain.rspace

import cats.Id
import coop.rchain.rspace.internal._

final case class Result[C, A](
    channel: C,
    matchedDatum: A,
    removedDatum: A,
    persistent: Boolean
)
final case class ContResult[C, P, K](
    continuation: K,
    persistent: Boolean,
    channels: Seq[C],
    patterns: Seq[P],
    peek: Boolean = false
)

/** The interface for RSpace
  *
  * @tparam C a type representing a channel
  * @tparam P a type representing a pattern
  * @tparam A a type representing an arbitrary piece of data and match result
  * @tparam K a type representing a continuation
  */
trait ISpace[F[_], C, P, A, K] extends Tuplespace[F, C, P, A, K] {

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

  def getJoins(channel: C): F[Seq[Seq[C]]]

  /** Clears the store.  Does not affect the history trie.
    */
  def clear(): F[Unit]

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
