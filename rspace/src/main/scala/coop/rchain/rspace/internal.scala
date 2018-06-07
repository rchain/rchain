package coop.rchain.rspace

import coop.rchain.scodec.codecs.seqOfN
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{bool, bytes, int32, int64, variableSizeBytesLong}

import scala.collection.immutable.Seq

object internal {

  final case class Datum[A](a: A, persist: Boolean)

  final case class DataCandidate[C, A](channel: C, datum: Datum[A], datumIndex: Int)

  final case class WaitingContinuation[P, K](patterns: Seq[P], continuation: K, persist: Boolean)

  final case class ProduceCandidate[C, P, A, K](channels: Seq[C],
                                                continuation: WaitingContinuation[P, K],
                                                continuationIndex: Int,
                                                dataCandidates: Seq[DataCandidate[C, A]])

  final case class Row[P, A, K](data: Seq[Datum[A]], wks: Seq[WaitingContinuation[P, K]])

  /** [[GNAT]] is not a `Tuple3`
    */
  final case class GNAT[C, P, A, K](
      channels: Seq[C],
      data: Seq[Datum[A]],
      wks: Seq[WaitingContinuation[P, K]]
  )

  sealed trait Operation extends Product with Serializable
  case object Insert     extends Operation
  case object Delete     extends Operation

  final case class TrieUpdate[C, P, A, K](count: Long,
                                          operation: Operation,
                                          channelsHash: Blake2b256Hash,
                                          gnat: GNAT[C, P, A, K])

  implicit val codecByteVector: Codec[ByteVector] =
    variableSizeBytesLong(int64, bytes)

  implicit def codecSeq[A](implicit codecA: Codec[A]): Codec[Seq[A]] =
    seqOfN(int32, codecA)

  implicit def codecDatum[A](implicit codecA: Codec[A]): Codec[Datum[A]] =
    (codecA :: bool).as[Datum[A]]

  implicit def codecWaitingContinuation[P, K](implicit
                                              codecP: Codec[P],
                                              codecK: Codec[K]): Codec[WaitingContinuation[P, K]] =
    (codecSeq(codecP) :: codecK :: bool).as[WaitingContinuation[P, K]]

  implicit def codecGNAT[C, P, A, K](implicit
                                     codecC: Codec[C],
                                     codecP: Codec[P],
                                     codecA: Codec[A],
                                     codecK: Codec[K]): Codec[GNAT[C, P, A, K]] =
    (codecSeq(codecC) ::
      codecSeq(codecDatum(codecA)) ::
      codecSeq(codecWaitingContinuation(codecP, codecK))).as[GNAT[C, P, A, K]]
}
