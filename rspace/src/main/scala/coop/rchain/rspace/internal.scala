package coop.rchain.rspace

import coop.rchain.shared.AttemptOps._
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{bool, bytes, int32, variableSizeBytes}
import coop.rchain.scodec.codecs.seqOfN

import scala.collection.immutable.Seq

object internal {

  final case class Datum[A](a: A, persist: Boolean)

  final case class DataCandidate[C, A](channel: C, datum: Datum[A], datumIndex: Int)

  final case class WaitingContinuation[P, K](patterns: Seq[P], continuation: K, persist: Boolean)

  final case class ProduceCandidate[C, P, A, K](channels: Seq[C],
                                                continuation: WaitingContinuation[P, K],
                                                continuationIndex: Int,
                                                dataCandidates: Seq[DataCandidate[C, A]])

  case class Row[P, A, K](data: Seq[Datum[A]], wks: Seq[WaitingContinuation[P, K]])

  /** [[GNAT]] is not a `Tuple3`
    */
  case class GNAT[C, P, A, K](
      channels: Seq[C],
      data: Seq[Datum[A]],
      wks: Seq[WaitingContinuation[P, K]]
  )

  private[rspace] case class WaitingContinuationBytes(patterns: Seq[ByteVector],
                                                      kvalue: ByteVector,
                                                      persist: Boolean)

  private[rspace] case class DatumBytes(datumBytes: ByteVector, persist: Boolean)

  private def fromAttempt[T](attempt: Attempt[DecodeResult[T]]): T =
    attempt.get.value

  def toBitVector[T](value: T, codec: Codec[T]): BitVector =
    codec.encode(value).toEither match {
      case Right(res) => res
      case Left(err)  => throw new Exception(err.toString)
    }

  def fromBitVector[T](vector: BitVector, codec: Codec[T]): T =
    fromAttempt(codec.decode(vector))

  lazy val codecByteVector: Codec[ByteVector] =
    variableSizeBytes(int32, bytes.xmap(x => x, x => x))

  lazy val codecSeqByteVector: Codec[Seq[ByteVector]] =
    seqOfN(int32, codecByteVector).as[Seq[ByteVector]]

  lazy val codecDatumBytes: Codec[DatumBytes] =
    (codecByteVector :: bool).as[DatumBytes]

  lazy val codecSeqDatumBytes: Codec[Seq[DatumBytes]] =
    seqOfN(int32, codecDatumBytes).as[Seq[DatumBytes]]

  lazy val codecWaitingContinuationBytes: Codec[WaitingContinuationBytes] =
    (codecSeqByteVector :: codecByteVector :: bool).as[WaitingContinuationBytes]

  lazy val codecSeqWaitingContinuationBytes: Codec[Seq[WaitingContinuationBytes]] =
    seqOfN(int32, codecWaitingContinuationBytes).as[Seq[WaitingContinuationBytes]]

  /* A new approach */

  implicit def codecDatum[A](implicit codecA: Codec[A]): Codec[Datum[A]] =
    (codecA :: bool).as[Datum[A]]

  implicit def codecWaitingContinuation[P, K](implicit
                                              codecP: Codec[P],
                                              codecK: Codec[K]): Codec[WaitingContinuation[P, K]] =
    (seqOfN(int32, codecP) :: codecK :: bool).as[WaitingContinuation[P, K]]

  implicit def codecGNAT[C, P, A, K](implicit
                                     codecC: Codec[C],
                                     codecP: Codec[P],
                                     codecA: Codec[A],
                                     codecK: Codec[K]): Codec[GNAT[C, P, A, K]] =
    (seqOfN(int32, codecC) ::
      seqOfN(int32, codecDatum(codecA)) ::
      seqOfN(int32, codecWaitingContinuation(codecP, codecK))).as[GNAT[C, P, A, K]]
}
