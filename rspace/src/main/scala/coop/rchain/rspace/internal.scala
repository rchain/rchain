package coop.rchain.rspace

import scala.collection.immutable.Seq
import coop.rchain.rspace.SeqCodec.seqOfN

object internal {

  final case class Datum[A](a: A, persist: Boolean)

  final case class DataCandidate[C, A](channel: C, datum: Datum[A], datumIndex: Int)

  final case class WaitingContinuation[P, K](patterns: Seq[P], continuation: K, persist: Boolean)

  final case class ProduceCandidate[C, P, A, K](channels: Seq[C],
                                                continuation: WaitingContinuation[P, K],
                                                continuationIndex: Int,
                                                dataCandidates: Seq[DataCandidate[C, A]])

  case class Row[P, A, K](data: Seq[Datum[A]], wks: Seq[WaitingContinuation[P, K]])

  private[rspace] object scodecs {

    import scodec.{Attempt, Codec, DecodeResult}
    import scodec.bits.{BitVector, ByteVector}
    import scodec.codecs.{bool, bytes, int32, variableSizeBytes}

    private[rspace] case class WaitingContinuationBytes(patterns: Seq[ByteVector],
                                                        kvalue: ByteVector,
                                                        persist: Boolean)

    private[rspace] case class DatumBytes(datumBytes: ByteVector, persist: Boolean)

    lazy val byteVectorCodec: Codec[ByteVector] =
      variableSizeBytes(int32, bytes.xmap(x => x, x => x))

    lazy val bytesListCodec: Codec[Seq[ByteVector]] =
      seqOfN(int32, byteVectorCodec).as[Seq[ByteVector]]

    lazy val datumBytesCodec: Codec[DatumBytes] = (byteVectorCodec :: bool).as[DatumBytes]

    lazy val dataBytesListCodec: Codec[Seq[DatumBytes]] =
      seqOfN(int32, datumBytesCodec).as[Seq[DatumBytes]]

    lazy val waitingContinuationBytesCodec: Codec[WaitingContinuationBytes] =
      (bytesListCodec :: byteVectorCodec :: bool).as[WaitingContinuationBytes]

    lazy val waitingContinuationBytesListCodec: Codec[Seq[WaitingContinuationBytes]] =
      seqOfN(int32, waitingContinuationBytesCodec).as[Seq[WaitingContinuationBytes]]

    private[this] def fromAttempt[T](attempt: Attempt[DecodeResult[T]]): T =
      attempt.toEither match {
        case Right(res) => res.value
        case Left(err)  => throw new Exception(err.toString)
      }

    def toBitVector[T](value: T, codec: Codec[T]): BitVector =
      codec.encode(value).toEither match {
        case Right(res) => res
        case Left(err)  => throw new Exception(err.toString)
      }

    def fromBitVector[T](vector: BitVector, codec: Codec[T]): T =
      fromAttempt(codec.decode(vector))
  }
}
