package coop.rchain.rspace

object internal {

  final case class Datum[A](a: A, persist: Boolean)

  final case class DataCandidate[C, A](channel: C, datum: Datum[A], datumIndex: Int)

  final case class WaitingContinuation[P, K](patterns: List[P], continuation: K, persist: Boolean)

  final case class ProduceCandidate[C, P, A, K](channels: List[C],
                                                continuation: WaitingContinuation[P, K],
                                                continuationIndex: Int,
                                                dataCandidates: List[DataCandidate[C, A]])

  case class Row[P, A, K](data: List[Datum[A]], wks: List[WaitingContinuation[P, K]])

  private[rspace] object scodecs {

    import scodec.{Attempt, Codec, DecodeResult}
    import scodec.bits.{BitVector, ByteVector}
    import scodec.codecs.{bool, bytes, int32, listOfN, variableSizeBytes}

    private[rspace] case class WaitingContinuationBytes(patterns: List[ByteVector],
                                                        kvalue: ByteVector,
                                                        persist: Boolean)

    private[rspace] case class DatumBytes(datumBytes: ByteVector, persist: Boolean)

    lazy val byteVectorCodec: Codec[ByteVector] =
      variableSizeBytes(int32, bytes.xmap(x => x, x => x))

    lazy val bytesListCodec: Codec[List[ByteVector]] =
      listOfN(int32, byteVectorCodec).as[List[ByteVector]]

    lazy val asBytesCodec: Codec[DatumBytes] = (byteVectorCodec :: bool).as[DatumBytes]

    lazy val asBytesListCodec: Codec[List[DatumBytes]] =
      listOfN(int32, asBytesCodec).as[List[DatumBytes]]

    lazy val psKsBytesCodec: Codec[WaitingContinuationBytes] =
      (bytesListCodec :: byteVectorCodec :: bool).as[WaitingContinuationBytes]

    lazy val waitingContinuationsBytesListCodec: Codec[List[WaitingContinuationBytes]] =
      listOfN(int32, psKsBytesCodec).as[List[WaitingContinuationBytes]]

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
