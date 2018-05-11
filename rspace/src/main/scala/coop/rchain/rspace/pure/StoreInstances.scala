package coop.rchain.rspace.pure

import java.nio.ByteBuffer

import cats.data.ReaderT
import cats.implicits._
import coop.rchain.catscontrib.Capture
import coop.rchain.rspace.Serialize
import coop.rchain.rspace.internal._
import org.lmdbjava.{CursorIterator, KeyRange, Txn}
import java.security.MessageDigest

import cats.Monad
import coop.rchain.rspace.util.{ignore, withResource}
import coop.rchain.rspace.internal.scodecs._
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}

import scala.collection.immutable.Seq
import coop.rchain.rspace.util

import scala.collection.JavaConverters._

object StoreInstances {

  implicit def storeLMDB[F[_], C, P, A, K](
      implicit
      serializeC: Serialize[C],
      serializeP: Serialize[P],
      serializeA: Serialize[A],
      serializeK: Serialize[K],
      captureF: Capture[F],
      monadF: Monad[F]): Store[ReaderT[F, LMDBContext, ?], C, P, A, K] =
    new Store[ReaderT[F, LMDBContext, ?], C, P, A, K] {

      private[this] def capture[X](x: X): F[X] = captureF.capture(x)

      type T = Txn[ByteBuffer]

      def createTxnRead(): ReaderT[F, LMDBContext, T] =
        ReaderT(ctx => capture(ctx.env.txnRead))

      def createTxnWrite(): ReaderT[F, LMDBContext, T] =
        ReaderT(ctx => capture(ctx.env.txnWrite))

      def withTxn[R](txn: T)(f: T => R): R =
        try {
          val ret: R = f(txn)
          txn.commit()
          ret
        } catch {
          case ex: Throwable =>
            txn.abort()
            throw ex
        } finally {
          txn.close()
        }

      type H = ByteBuffer

      private[this] def hashChannelsInner(channels: Seq[C]): ByteBuffer =
        hashBytes(toByteBuffer(channels))

      def hashChannels(channels: Seq[C]): ReaderT[F, LMDBContext, ByteBuffer] =
        ReaderT.pure(hashChannelsInner(channels))

      def getChannels(txn: T, channelsHash: H): ReaderT[F, LMDBContext, Seq[C]] =
        ReaderT { ctx =>
          capture {
            Option(ctx.dbKeys.get(txn, channelsHash))
              .map(fromByteBuffer[C])
              .getOrElse(Seq.empty[C])
          }
        }

      private[rspace] def putChannels(txn: T, channels: Seq[C]): ReaderT[F, LMDBContext, H] =
        ReaderT { ctx =>
          capture {
            val channelsBytes = toByteBuffer(channels)
            val channelsHash  = hashBytes(channelsBytes)
            ctx.dbKeys.put(txn, channelsHash, channelsBytes)
            channelsHash
          }
        }

      private[this] def readDatumByteses(
          txn: T,
          channelsHash: H): ReaderT[F, LMDBContext, Option[Seq[DatumBytes]]] =
        ReaderT { ctx =>
          capture {
            Option(ctx.dbData.get(txn, channelsHash)).map(fromByteBuffer(_, datumBytesesCodec))
          }
        }

      private[this] def writeDatumByteses(txn: T,
                                          channelsHash: H,
                                          values: Seq[DatumBytes]): ReaderT[F, LMDBContext, Unit] =
        ReaderT { ctx =>
          capture {
            if (values.nonEmpty) {
              ctx.dbData.put(txn, channelsHash, toByteBuffer(values, datumBytesesCodec))
            } else {
              ctx.dbData.delete(txn, channelsHash)
              collectGarbage(txn, channelsHash, dataCollected = true)
            }
          }
        }

      def getData(txn: T, channels: Seq[C]): ReaderT[F, LMDBContext, Seq[Datum[A]]] =
        hashChannels(channels)
          .flatMap(readDatumByteses(txn, _))
          .map {
            _.map(_.map(bytes => Datum(fromByteVector[A](bytes.datumBytes), bytes.persist)))
              .getOrElse(Seq.empty[Datum[A]])
          }

      def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): ReaderT[F, LMDBContext, Unit] =
        for {
          channelsHash    <- putChannels(txn, channels)
          oldDatumByteses <- readDatumByteses(txn, channelsHash)
          newDatumBytes   = DatumBytes(toByteVector(datum.a), datum.persist)
          _ <- writeDatumByteses(txn,
                                 channelsHash,
                                 newDatumBytes +: oldDatumByteses.getOrElse(Seq.empty[DatumBytes]))
        } yield ()

      def removeDatum(txn: T, channel: C, index: Int): ReaderT[F, LMDBContext, Unit] =
        removeDatum(txn, Seq(channel), index)

      def removeDatum(txn: T, channels: Seq[C], index: Int): ReaderT[F, LMDBContext, Unit] =
        for {
          channelsHash <- hashChannels(channels)
          datumByteses <- readDatumByteses(txn, channelsHash)
        } yield
          datumByteses match {
            case Some(datumByteses) =>
              writeDatumByteses(txn, channelsHash, util.dropIndex(datumByteses, index))
            case None =>
              captureF.fail(new IllegalArgumentException(s"removeDatum: no values at $channels"))
          }

      private[this] def readWaitingContinuationByteses(
          txn: T,
          channelsHash: H): ReaderT[F, LMDBContext, Option[Seq[WaitingContinuationBytes]]] =
        ReaderT { ctx =>
          capture {
            Option(ctx.dbWaitingContinuations.get(txn, channelsHash))
              .map(fromByteBuffer(_, waitingContinuationsSeqCodec))
          }
        }

      private[this] def writeWaitingContinuationByteses(
          txn: T,
          channelsHash: H,
          values: Seq[WaitingContinuationBytes]): ReaderT[F, LMDBContext, Unit] =
        ReaderT { ctx =>
          capture {
            if (values.nonEmpty) {
              ctx.dbWaitingContinuations.put(txn,
                                             channelsHash,
                                             toByteBuffer(values, waitingContinuationsSeqCodec))
            } else {
              ctx.dbWaitingContinuations.delete(txn, channelsHash)
              collectGarbage(txn, channelsHash, waitingContinuationsCollected = true)
            }
          }
        }

      def getWaitingContinuation(
          txn: T,
          channels: Seq[C]): ReaderT[F, LMDBContext, Seq[WaitingContinuation[P, K]]] =
        for {
          channelsHash                    <- hashChannels(channels)
          maybeWaitingContinuationByteses <- readWaitingContinuationByteses(txn, channelsHash)
        } yield
          maybeWaitingContinuationByteses match {
            case Some(waitingContinuationsByteses) =>
              waitingContinuationsByteses.map { waitingContinuationBytes =>
                WaitingContinuation(fromByteVectors[P](waitingContinuationBytes.patterns),
                                    fromByteVector[K](waitingContinuationBytes.kvalue),
                                    waitingContinuationBytes.persist)
              }
            case None =>
              Seq.empty[WaitingContinuation[P, K]]
          }

      def removeWaitingContinuation(txn: T,
                                    channels: Seq[C],
                                    index: Int): ReaderT[F, LMDBContext, Unit] =
        for {
          channelsHash                <- hashChannels(channels)
          waitingContinuationsByteses <- readWaitingContinuationByteses(txn, channelsHash)
        } yield
          waitingContinuationsByteses match {
            case Some(waitingContinuationByteses) =>
              writeWaitingContinuationByteses(txn,
                                              channelsHash,
                                              util.dropIndex(waitingContinuationByteses, index))
            case None =>
              captureF.fail(
                new IllegalArgumentException(s"removeWaitingContinuation: no values at $channels"))
          }

      def putWaitingContinuation(
          txn: T,
          channels: Seq[C],
          continuation: WaitingContinuation[P, K]): ReaderT[F, LMDBContext, Unit] =
        for {
          channelsHash            <- putChannels(txn, channels)
          oldWaitingContinuations <- readWaitingContinuationByteses(txn, channelsHash)
          newWaitingContinuation = WaitingContinuationBytes(toByteVectorSeq(continuation.patterns),
                                                            toByteVector(continuation.continuation),
                                                            continuation.persist)
          _ <- writeWaitingContinuationByteses(
                txn,
                channelsHash,
                newWaitingContinuation +: oldWaitingContinuations.getOrElse(
                  Seq.empty[WaitingContinuationBytes]))
        } yield ()

      def removeJoin(txn: T, channel: C, channels: Seq[C]): ReaderT[F, LMDBContext, Unit] =
        ReaderT { ctx =>
          capture {
            val joinKey = hashChannelsInner(Seq(channel))
            Option(ctx.dbJoins.get(txn, joinKey))
              .map(toByteVectors)
              .map(_.map(fromByteVectors[C]))
              .map(exSeq => (exSeq, exSeq.indexOf(channels)))
              .map {
                case (exSeq, idx) =>
                  if (idx >= 0) {
                    val channelsHash = hashChannelsInner(channels)
                    if (ctx.dbWaitingContinuations.get(txn, channelsHash) == null) {
                      val resSeq = util.dropIndex(exSeq, idx)
                      if (resSeq.nonEmpty) {
                        ctx.dbJoins.put(txn, joinKey, toByteBuffer(resSeq.map(toByteVectorSeq(_))))
                      } else {
                        ctx.dbJoins.delete(txn, joinKey)
                        collectGarbage(txn, joinKey, joinsCollected = true)
                      }
                    }
                  } else {
                    throw new IllegalArgumentException(
                      s"removeJoin: $channels is not a member of $exSeq")
                  }
              }
              .getOrElse(())
          }
        }

      def removeAllJoins(txn: T, channel: C): ReaderT[F, LMDBContext, Unit] =
        ReaderT { ctx =>
          capture {
            val joinKey = hashChannelsInner(Seq(channel))
            ctx.dbJoins.delete(txn, joinKey)
            collectGarbage(txn, joinKey)
          }
        }

      def putJoin(txn: T, channel: C, channels: Seq[C]): ReaderT[F, LMDBContext, Unit] =
        ReaderT { ctx =>
          capture {
            val joinKey = hashChannelsInner(Seq(channel))
            val oldJoinsBv =
              Option(ctx.dbJoins.get(txn, joinKey))
                .map(toByteVectors)
                .getOrElse(Seq.empty[Seq[ByteVector]])

            val newJoin = toByteVectorSeq(channels)
            if (!oldJoinsBv.contains(newJoin)) {
              ctx.dbJoins.put(txn, joinKey, toByteBuffer(newJoin +: oldJoinsBv))
            }
          }
        }

      def getJoin(txn: T, channel: C): ReaderT[F, LMDBContext, Seq[Seq[C]]] =
        ReaderT { ctx =>
          capture {
            val joinKey = hashChannelsInner(Seq(channel))
            Option(ctx.dbJoins.get(txn, joinKey))
              .map(toByteVectors)
              .map(_.map(fromByteVectors[C]))
              .getOrElse(Seq.empty[Seq[C]])
          }
        }

      def removeAll(txn: T, channels: Seq[C]): ReaderT[F, LMDBContext, Unit] =
        hashChannels(channels)
          .flatMap { channelsHash =>
            {
              readWaitingContinuationByteses(txn, channelsHash)
                .map(_ => writeWaitingContinuationByteses(txn, channelsHash, Seq.empty))

              readDatumByteses(txn, channelsHash)
                .map(_ => writeDatumByteses(txn, channelsHash, Seq.empty))
            }
          }
          .map(_ => channels.foreach(c => removeJoin(txn, c, channels)))

      def toMap: ReaderT[F, LMDBContext, Map[Seq[C], Row[P, A, K]]] = {
        def getIterator(txn: Txn[ByteBuffer]): ReaderT[F, LMDBContext, CursorIterator[ByteBuffer]] =
          ReaderT { ctx: LMDBContext =>
            capture {
              val keyRange: KeyRange[ByteBuffer] = KeyRange.all()
              ctx.dbKeys.iterate(txn, keyRange)
            }
          }

        def go(txn: Txn[ByteBuffer]): ReaderT[F, LMDBContext, List[(Seq[C], Row[P, A, K])]] =
          withTxn(txn) { txn =>
            getIterator(txn).flatMap { iterator =>
              withResource(iterator) { (it: CursorIterator[ByteBuffer]) =>
                it.asScala.toList.traverse[ReaderT[F, LMDBContext, ?], (Seq[C], Row[P, A, K])] {
                  (ci: CursorIterator.KeyVal[ByteBuffer]) =>
                    for {
                      channels             <- getChannels(txn, ci.key())
                      data                 <- getData(txn, channels)
                      waitingContinuations <- getWaitingContinuation(txn, channels)
                    } yield channels -> Row(data, waitingContinuations)
                }
              }
            }
          }

        for {
          txn <- createTxnRead()
          res <- go(txn)
        } yield res.toMap
      }

      def close(): ReaderT[F, LMDBContext, Unit] =
        ReaderT { ctx =>
          capture { ctx.close() }
        }

      def clear(): ReaderT[F, LMDBContext, Unit] =
        createTxnWrite().flatMap(txn =>
          withTxn(txn) { txn =>
            ReaderT { ctx =>
              capture {
                ctx.dbKeys.drop(txn)
                ctx.dbData.drop(txn)
                ctx.dbWaitingContinuations.drop(txn)
                ctx.dbJoins.drop(txn)
                ()
              }
            }
        })

      def collectGarbage(txn: T,
                         channelsHash: H,
                         dataCollected: Boolean = false,
                         waitingContinuationsCollected: Boolean = false,
                         joinsCollected: Boolean = false): Unit = {}
    }

  private[rspace] def toByteVector[T](value: T)(implicit st: Serialize[T]): ByteVector =
    ByteVector(st.encode(value))

  private[rspace] def fromByteVector[T](vector: ByteVector)(implicit st: Serialize[T]): T =
    st.decode(vector.toArray) match {
      case Left(err)     => throw new Exception(err)
      case Right(result) => result
    }

  private[rspace] def toByteBuffer[T](value: T, codec: Codec[T]): ByteBuffer =
    toByteBuffer(toBitVector(value, codec))

  private[rspace] def toByteBuffer[T](values: Seq[T])(implicit st: Serialize[T]): ByteBuffer =
    toByteBuffer(toBitVector(toByteVectorSeq(values), byteVectorsCodec))

  private[rspace] def toByteBuffer(vector: BitVector): ByteBuffer = {
    val bytes          = vector.bytes
    val bb: ByteBuffer = ByteBuffer.allocateDirect(bytes.size.toInt)
    bytes.copyToBuffer(bb)
    bb.flip()
    bb
  }

  private[rspace] def toByteVectorSeq[T](values: Seq[T])(
      implicit st: Serialize[T]): Seq[ByteVector] =
    values.map(st.encode).map(ByteVector(_))

  private[rspace] def fromByteVectors[T](vectors: Seq[ByteVector])(
      implicit st: Serialize[T]): Seq[T] =
    vectors
      .map(_.toArray)
      .map(st.decode)
      .map {
        case Left(err)     => throw new Exception(err)
        case Right(values) => values
      }

  private[rspace] def toByteVectors(byteBuffer: ByteBuffer): Seq[Seq[ByteVector]] =
    fromBitVector(BitVector(byteBuffer), byteVectorsCodec)
      .map(x => fromBitVector(x.bits, byteVectorsCodec))

  private[rspace] def toByteBuffer(vectors: Seq[Seq[ByteVector]]): ByteBuffer = {
    val bl = vectors.map(toBitVector(_, byteVectorsCodec).toByteVector)
    toByteBuffer(bl, byteVectorsCodec)
  }

  private[rspace] def fromByteBuffer[T](byteBuffer: ByteBuffer)(implicit st: Serialize[T]): Seq[T] =
    fromBitVector(BitVector(byteBuffer), byteVectorsCodec)
      .map(_.toArray)
      .map(st.decode)
      .map {
        case Left(err)     => throw new Exception(err)
        case Right(values) => values
      }

  private[rspace] def fromByteBuffer[T](byteBuffer: ByteBuffer, codec: Codec[T]): T =
    fromBitVector(BitVector(byteBuffer), codec)

  private[rspace] def hashBytes(byteBuffer: ByteBuffer): ByteBuffer = {
    byteBuffer.mark()
    val fetched = new Array[Byte](byteBuffer.remaining())
    ignore {
      byteBuffer.get(fetched)
    }
    byteBuffer.reset()
    hashBytes(fetched)
  }

  private[rspace] def hashBytes(bytes: Array[Byte]): ByteBuffer = {
    val dataArr    = MessageDigest.getInstance("SHA-256").digest(bytes)
    val byteBuffer = ByteBuffer.allocateDirect(dataArr.length)
    byteBuffer.put(dataArr).flip()
    byteBuffer
  }
}
