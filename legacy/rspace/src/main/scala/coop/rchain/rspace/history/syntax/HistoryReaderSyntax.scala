package coop.rchain.rspace.history.syntax

import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history._
import coop.rchain.rspace.serializers.ScodecSerialize.{DatumB, JoinsB, WaitingContinuationB}

trait HistoryReaderSyntax {
  implicit final def rspaceSyntaxHistoryReader[F[_], C, P, A, K](
      historyReader: HistoryReader[F, Blake2b256Hash, C, P, A, K]
  ): HistoryReaderOps[F, C, P, A, K] =
    new HistoryReaderOps[F, C, P, A, K](historyReader)
}

final class HistoryReaderOps[F[_], C, P, A, K](
    private val historyReader: HistoryReader[F, Blake2b256Hash, C, P, A, K]
) extends AnyVal {

  /**
    * Reader with binary data included in result
    *
    * These methods returning raw bytes along with decode value is performance optimization.
    * Making diff for two [[WaitingContinuation]] is 5-10 times slower then in [[ByteVector]] form,
    * so the binary value is used as equality comparison for class instance.
    */
  def readerBinary: HistoryReaderBinary[F, C, P, A, K] =
    new HistoryReaderBinary[F, C, P, A, K] {
      override def getData(key: Blake2b256Hash): F[Seq[DatumB[A]]] =
        historyReader.getDataProj(key)(DatumB(_, _))

      override def getContinuations(key: Blake2b256Hash): F[Seq[WaitingContinuationB[P, K]]] =
        historyReader.getContinuationsProj(key)(WaitingContinuationB(_, _))

      override def getJoins(key: Blake2b256Hash): F[Seq[JoinsB[C]]] =
        historyReader.getJoinsProj(key)(JoinsB(_, _))
    }
}
