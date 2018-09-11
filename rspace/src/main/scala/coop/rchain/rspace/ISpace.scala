package coop.rchain.rspace

import coop.rchain.rspace.internal._

import scala.collection.immutable.Seq

/** The interface for RSpace
  *
  * @tparam C a type representing a channel
  * @tparam P a type representing a pattern
  * @tparam A a type representing an arbitrary piece of data
  * @tparam R a type representing a match result
  * @tparam K a type representing a continuation
  */
trait ISpace[F[_], C, P, A, R, K] {

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
  def consume(channels: Seq[C], patterns: Seq[P], continuation: K, persist: Boolean)(
      implicit m: Match[P, A, R]): F[Option[(K, Seq[R])]]

  def install(channels: Seq[C], patterns: Seq[P], continuation: K)(
      implicit m: Match[P, A, R]): F[Option[(K, Seq[R])]]

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
  def produce(channel: C, data: A, persist: Boolean)(
      implicit m: Match[P, A, R]): F[Option[(K, Seq[R])]]

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

  /**
    * Retrieves a GNAT from the history trie at a particular checkpoint and channels hash.
    */
  def retrieve(root: Blake2b256Hash, channelsHash: Blake2b256Hash): F[Option[GNAT[C, P, A, K]]]

  /** Clears the store.  Does not affect the history trie.
    */
  def clear(): F[Unit]

  /** Closes
    */
  def close(): F[Unit]
}
