package coop.rchain.casper

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.codecs
import coop.rchain.metrics.Span
import coop.rchain.rspace.ReportingRspace._
import coop.rchain.scodec.codecs.seqOfN
import coop.rchain.shared.AttemptOpsF.RichAttempt
import coop.rchain.shared.Serialize
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStoreManager, KeyValueTypedStore}
import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs.{discriminated, int32, uint2}

/**
  This is a temporary solution for the performance problem which is caused by the
  reporting api. Exchanges highly rely on this api to get transactions info.But
  every time request to the event data, the node would replay the block once again
  which makes the api so slow and can not processed concurrently.

  The ReportHotStore is a in memory store for event data in every block.
  */
trait ReportMemStore[F[_]] {
  def put(hash: ByteString, data: Seq[Seq[ReportingEvent]]): F[Unit]
  def get(hash: ByteString): F[Option[Seq[Seq[ReportingEvent]]]]
}

class ReportMemStoreImpl[F[_]: Sync](
    store: KeyValueTypedStore[F, ByteString, BitVector],
    coder: Codec[Seq[Seq[ReportingEvent]]]
) extends ReportMemStore[F] {
  override def get(hash: ByteString): F[Option[Seq[Seq[ReportingEvent]]]] = {
    import cats.instances.option._
    for {
      encoded <- store.get(hash)
      data    <- encoded.traverse(e => coder.decode(e).get)
      result  = data.map(_.value)
    } yield result
  }

  override def put(hash: ByteString, data: Seq[Seq[ReportingEvent]]): F[Unit] =
    for {
      encoded <- coder.encode(data).get
      _       <- store.put(hash, encoded)
    } yield ()
}

object ReportMemStore {
  def codecSeq[A](codecA: Codec[A]): Codec[Seq[A]] =
    seqOfN(int32, codecA)
  implicit def codecReportingProduce[C, A](
      implicit codeC: Codec[C],
      codeA: Codec[A]
  ): Codec[ReportingProduce[C, A]] =
    (codeC :: codeA).as[ReportingProduce[C, A]]
  implicit def codecReportingConsume[C, P, K](
      implicit codeC: Codec[C],
      codeP: Codec[P],
      codeK: Codec[K]
  ): Codec[ReportingConsume[C, P, K]] =
    (codecSeq(codeC) :: codecSeq(codeP) :: codeK :: codecSeq(int32))
      .as[ReportingConsume[C, P, K]]
  implicit def codecReportingComm[C, P, A, K](
      implicit codecC: Codec[C],
      codecP: Codec[P],
      codecA: Codec[A],
      codecK: Codec[K]
  ): Codec[ReportingComm[C, P, A, K]] =
    (codecReportingConsume[C, P, K](codecC, codecP, codecK) :: codecSeq(
      codecReportingProduce[C, A](codecC, codecA)
    )).as[ReportingComm[C, P, A, K]]

  def reportingCodec[C, P, A, K](
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeA: Serialize[A],
      serializeK: Serialize[K]
  ): Codec[ReportingEvent] =
    discriminated[ReportingEvent]
      .by(uint2)
      .subcaseP(0)({
        case n: ReportingProduce[C, A] @unchecked => n
      })(codecReportingProduce(serializeC.toSizeHeadCodec, serializeA.toSizeHeadCodec))
      .subcaseP(1) {
        case n: ReportingConsume[C, P, K] @unchecked => n
      }(
        codecReportingConsume(
          serializeC.toSizeHeadCodec,
          serializeP.toSizeHeadCodec,
          serializeK.toSizeHeadCodec
        )
      )
      .subcaseP(2) {
        case n: ReportingComm[C, P, A, K] @unchecked => n
      }(
        codecReportingComm(
          serializeC.toSizeHeadCodec,
          serializeP.toSizeHeadCodec,
          serializeA.toSizeHeadCodec,
          serializeK.toSizeHeadCodec
        )
      )

  def store[F[_]: Sync: Span, C, P, A, K](kvm: KeyValueStoreManager[F])(
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeA: Serialize[A],
      serializeK: Serialize[K]
  ): F[ReportMemStore[F]] = {
    val codecReporting = reportingCodec[C, P, A, K]
    val codec          = codecSeq[ReportingEvent](codecReporting)
    val seqEventCodec  = codecSeq[Seq[ReportingEvent]](codec)

    for {
      db <- kvm.database("reporting-cache", codecs.codecByteString, scodec.codecs.bits)
    } yield new ReportMemStoreImpl[F](db, seqEventCodec)
  }
}
