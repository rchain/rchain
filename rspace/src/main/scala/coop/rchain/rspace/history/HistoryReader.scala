package coop.rchain.rspace.history

import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.internal._
import coop.rchain.rspace.serializers.ScodecSerialize.{RichDatum, RichJoin, RichKont}
import scodec.bits.ByteVector

/**
  * Reader for particular history (state verified on blockchain)
  *
  * @tparam F effect type
  * @tparam Key type for hash of a channel
  * @tparam C type for Channel => this is Par
  * @tparam P type for Pattern => this is BindPattern
  * @tparam A type for Abstraction => this is ListParWithRandom
  * @tparam K type for Continuation => this is TaggedContinuation
  */
trait HistoryReader[F[_], Key, C, P, A, K] {

  // Get current root which reader reads from
  def root: Key

  def getDataProj[R](key: Key)(proj: (Datum[A], ByteVector) => R): F[Seq[R]]

  def getContinuationsProj[R](key: Key)(
      proj: (WaitingContinuation[P, K], ByteVector) => R
  ): F[Seq[R]]

  def getJoinsProj[R](key: Key)(proj: (Seq[C], ByteVector) => R): F[Seq[R]]

  /**
    * Defaults
    */
  def getData(key: Key): F[Seq[Datum[A]]] =
    getDataProj(key)((d, _) => d)

  def getContinuations(key: Key): F[Seq[WaitingContinuation[P, K]]] =
    getContinuationsProj(key)((d, _) => d)

  def getJoins(key: Key): F[Seq[Seq[C]]] =
    getJoinsProj(key)((d, _) => d)

  /**
    * Get reader which accepts non-serialized and hashed keys
    */
  def base: HistoryReaderBase[F, C, P, A, K]
}

/**
  * History reader base, version of a reader which accepts non-serialized and hashed keys
  */
trait HistoryReaderBase[F[_], C, P, A, K] {
  def getDataProj[R](key: C): ((Datum[A], ByteVector) => R) => F[Seq[R]]

  def getContinuationsProj[R](
      key: Seq[C]
  ): ((WaitingContinuation[P, K], ByteVector) => R) => F[Seq[R]]

  def getJoinsProj[R](key: C): ((Seq[C], ByteVector) => R) => F[Seq[R]]

  /**
    * Defaults
    */
  def getData(key: C): F[Seq[Datum[A]]] =
    getDataProj(key)((d, _) => d)

  def getContinuations(key: Seq[C]): F[Seq[WaitingContinuation[P, K]]] =
    getContinuationsProj(key)((d, _) => d)

  def getJoins(key: C): F[Seq[Seq[C]]] =
    getJoinsProj(key)((d, _) => d)
}

/**
  * History reader with binary data included in result
  */
trait HistoryReaderBinary[F[_], C, P, A, K] {
  def getData(key: Blake2b256Hash): F[Seq[RichDatum[A]]]

  def getContinuations(key: Blake2b256Hash): F[Seq[RichKont[P, K]]]

  def getJoins(key: Blake2b256Hash): F[Seq[RichJoin[C]]]
}
