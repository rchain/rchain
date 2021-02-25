package coop.rchain.rspace.history

import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.internal._
import coop.rchain.shared.Serialize
import scodec.Codec

/**
  * Reader for particular history (state verified on blockchain)
  *
  * @tparam F effect type
  * @tparam C type for Channel => this is Par
  * @tparam P type for Pattern => this is BindPattern
  * @tparam A type for Abstraction => this is ListParWithRandom
  * @tparam K type for Continuation => this is TaggedContinuation
  */
trait HistoryReader[F[_], C, P, A, K, DK, JK, CK] {

  // get content at a chanel
  def getData(key: DK): F[Seq[Datum[A]]]

  def getJoins(key: JK): F[Seq[Seq[C]]]

  def getContinuations(key: CK): F[Seq[WaitingContinuation[P, K]]]

  // get flavor for rich content (with raw ByteVector representation for each item)
  def getRichDatums(key: DK): F[Seq[RichDatum[A]]]

  def getRichJoins(key: JK): F[Seq[RichJoin[C]]]

  def getRichContinuations(key: CK): F[Seq[RichKont[P, K]]]

  // get current root which reader reads from
  def root: Blake2b256Hash

}

/**
  * Reader for history with addressing by Blake256Hash key
  */
trait HashHistoryReader[F[_], C, P, A, K]
    extends HistoryReader[F, C, P, A, K, Blake2b256Hash, Blake2b256Hash, Blake2b256Hash] {

  /**
    * transform reader into [[RhoHistoryReader]]
    *
    * @param codecC codec for serializing channel
    * @return
    */
  def toRho(
      implicit codecC: Codec[C],
      sync: Sync[F]
  ): RhoHistoryReader[F, C, P, A, K] = {
    import coop.rchain.rspace.syntax._
    val hashReader          = this
    implicit val serializeC = Serialize.fromCodec(codecC)
    new RhoHistoryReader[F, C, P, A, K] {
      override def getData(key: C): F[Seq[Datum[A]]] = hashReader.getData(key)

      override def getJoins(key: C): F[Seq[Seq[C]]] = hashReader.getJoins(key)

      override def getContinuations(key: Seq[C]): F[Seq[WaitingContinuation[P, K]]] =
        hashReader.getContinuations(key)

      override def getRichDatums(key: C): F[Seq[RichDatum[A]]] = hashReader.getRichData(key)

      override def getRichJoins(key: C): F[Seq[RichJoin[C]]] = hashReader.getRichJoins(key)

      override def getRichContinuations(key: Seq[C]): F[Seq[RichKont[P, K]]] =
        this.getRichContinuations(key)

      override def root: Blake2b256Hash = hashReader.root
    }
  }
}

/**
  * Reader for history with addressing by Rholang channels.
  */
trait RhoHistoryReader[F[_], C, P, A, K] extends HistoryReader[F, C, P, A, K, C, C, Seq[C]]
