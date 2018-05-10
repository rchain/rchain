package coop.rchain.rspace.pure

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.concurrent.locks.StampedLock
import javax.xml.bind.DatatypeConverter.printHexBinary

import cats.implicits._
import cats.Monad
import cats.data.ReaderT
import coop.rchain.catscontrib.Capture
import coop.rchain.rspace.Serialize
import coop.rchain.rspace.examples.makeSerializeFromSerializable
import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}
import coop.rchain.rspace.util.dropIndex

import scala.collection.immutable.Seq
import scala.collection.mutable

class InMemoryStoreInstance {

  import InMemoryStoreInstance._

  class InMemoryContext[C, P, A, K] {
    private[rspace] val lock = new StampedLock()

    private[rspace] val _keys = mutable.HashMap.empty[String, Seq[C]]
    private[rspace] val _waitingContinuations =
      mutable.HashMap.empty[String, Seq[WaitingContinuation[P, K]]]
    private[rspace] val _data = mutable.HashMap.empty[String, Seq[Datum[A]]]
    private[rspace] val _joinMap = new mutable.HashMap[C, mutable.Set[String]]
    with mutable.MultiMap[C, String]
  }

  def storeInMemory[F[_], C, P, A, K <: Serializable](
      implicit
      serializeC: Serialize[C],
      captureF: Capture[F],
      monadF: Monad[F]
  ): Store[ReaderT[F, InMemoryContext[C, P, A, K], ?], C, P, A, K] = {
    type InMemoryCtx = InMemoryContext[C, P, A, K]

    new Store[ReaderT[F, InMemoryCtx, ?], C, P, A, K] {
      private[this] def capture[X](x: X): F[X] = captureF.capture(x)

      override type H = String

      override type T = (InMemoryCtx, Long)

      override def createTxnRead(): ReaderT[F, InMemoryCtx, T] =
        ReaderT(ctx => capture((ctx, ctx.lock.readLock())))

      override def createTxnWrite(): ReaderT[F, InMemoryCtx, T] =
        ReaderT(ctx => capture((ctx, ctx.lock.writeLock())))

      override def withTxn[R](txn: T)(f: T => R): R =
        try {
          f(txn)
        } finally {
          val (ctx, value) = txn
          ctx.lock.unlock(value)
        }

      private[this] def hashChannelsInner(channels: Seq[C]): H =
        printHexBinary(hashBytes(channels.flatMap(serializeC.encode).toArray))

      def hashChannels(channels: Seq[C]): ReaderT[F, InMemoryCtx, H] =
        ReaderT.pure(hashChannelsInner(channels))

      def getChannels(txn: T, channelsHash: H): ReaderT[F, InMemoryCtx, Seq[C]] =
        ReaderT { ctx =>
          capture {
            ctx._keys.getOrElse(channelsHash, Seq.empty[C])
          }
        }

      private[rspace] def putChannels(txn: T, channels: Seq[C]): ReaderT[F, InMemoryCtx, Unit] =
        ReaderT { ctx =>
          capture {
            ctx._keys.update(hashChannelsInner(channels), channels)
          }
        }

      def getData(txn: T, channels: Seq[C]): ReaderT[F, InMemoryCtx, Seq[Datum[A]]] =
        ReaderT { ctx =>
          capture {
            ctx._data.getOrElse(hashChannelsInner(channels), Seq.empty[Datum[A]])
          }
        }

      def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): ReaderT[F, InMemoryCtx, Unit] =
        ReaderT { ctx =>
          capture {
            val channelsHash = hashChannelsInner(channels)
            putChannels(txn, channels).map(_ => {
              val datums = ctx._data.getOrElseUpdate(channelsHash, Seq.empty[Datum[A]])
              ctx._data.update(channelsHash, datum +: datums)
            })
          }
        }

      def removeDatum(txn: T, channel: C, index: Int): ReaderT[F, InMemoryCtx, Unit] =
        removeDatum(txn, Seq(channel), index)

      def removeDatum(txn: T, channels: Seq[C], index: Int): ReaderT[F, InMemoryCtx, Unit] =
        ReaderT { ctx =>
          capture {
            val channelsHash = hashChannelsInner(channels)
            for (data <- ctx._data.get(channelsHash)) {
              ctx._data.update(channelsHash, dropIndex(data, index))
            }
            collectGarbage(ctx, channelsHash)
          }
        }

      def getWaitingContinuation(
          txn: T,
          channels: Seq[C]): ReaderT[F, InMemoryCtx, Seq[WaitingContinuation[P, K]]] =
        ReaderT { ctx =>
          capture {
            ctx._waitingContinuations
              .getOrElse(hashChannelsInner(channels), Seq.empty[WaitingContinuation[P, K]])
              .map { (wk: WaitingContinuation[P, K]) =>
                wk.copy(continuation = InMemoryStoreInstance.roundTrip(wk.continuation))
              }
          }
        }

      def removeWaitingContinuation(txn: T,
                                    channels: Seq[C],
                                    index: Int): ReaderT[F, InMemoryCtx, Unit] =
        ReaderT { ctx =>
          capture {
            val channelsHash = hashChannelsInner(channels)
            for (waitingContinuation <- ctx._waitingContinuations.get(channelsHash)) {
              ctx._waitingContinuations.update(channelsHash, dropIndex(waitingContinuation, index))
            }
            collectGarbage(ctx, channelsHash)
          }
        }

      def putWaitingContinuation(
          txn: T,
          channels: Seq[C],
          continuation: WaitingContinuation[P, K]): ReaderT[F, InMemoryCtx, Unit] =
        ReaderT { ctx =>
          capture {
            val channelsHash = hashChannelsInner(channels)
            putChannels(txn, channels).map(_ => {
              val waitingContinuations =
                ctx._waitingContinuations.getOrElseUpdate(channelsHash,
                                                          Seq.empty[WaitingContinuation[P, K]])
              ctx._waitingContinuations.update(channelsHash, waitingContinuations :+ continuation)
            })
          }
        }

      def removeJoin(txn: T, channel: C, channels: Seq[C]): ReaderT[F, InMemoryCtx, Unit] =
        ReaderT { ctx =>
          capture {
            val joinKey      = hashChannelsInner(Seq(channel))
            val channelsHash = hashChannelsInner(channels)
            if (ctx._waitingContinuations.get(channelsHash).forall(_.isEmpty)) {
              ctx._joinMap.removeBinding(channel, channelsHash)
            }
            collectGarbage(ctx, joinKey)
          }
        }

      def removeAllJoins(txn: T, channel: C): ReaderT[F, InMemoryCtx, Unit] =
        ReaderT { ctx =>
          capture {
            ctx._joinMap.remove(channel)
            collectGarbage(ctx, hashChannelsInner(Seq(channel)))
          }
        }

      def putJoin(txn: T, channel: C, channels: Seq[C]): ReaderT[F, InMemoryCtx, Unit] =
        ReaderT { ctx =>
          capture {
            ctx._joinMap.addBinding(channel, hashChannelsInner(channels))
          }
        }

      def getJoin(txn: T, channel: C): ReaderT[F, InMemoryCtx, Seq[Seq[C]]] =
        ReaderT { ctx =>
          capture {
            ctx._joinMap
              .getOrElse(channel, Set.empty[String])
              .toList
              .map(ctx._keys.getOrElse(_, Seq.empty[C]))
          }
        }

      def removeAll(txn: T, channels: Seq[C]): ReaderT[F, InMemoryCtx, Unit] =
        ReaderT { ctx =>
          capture {
            val channelsHash = hashChannelsInner(channels)
            ctx._data.put(channelsHash, Seq.empty)
            ctx._waitingContinuations.put(channelsHash, Seq.empty)
            for (c <- channels)
              removeJoin(txn, c, channels).run(ctx)
          }
        }

      def toMap: ReaderT[F, InMemoryCtx, Map[Seq[C], Row[P, A, K]]] =
        ReaderT { ctx =>
          capture {
            ctx._keys.map {
              case (channelsHash, channels) =>
                val data = ctx._data.getOrElse(channelsHash, Seq.empty[Datum[A]])
                val waitingContinuations =
                  ctx._waitingContinuations.getOrElse(channelsHash,
                                                      Seq.empty[WaitingContinuation[P, K]])
                (channels, Row(data, waitingContinuations))
            }.toMap
          }
        }

      def close(): ReaderT[F, InMemoryCtx, Unit] = ReaderT.pure(())

      private[rspace] def clear(): ReaderT[F, InMemoryCtx, Unit] =
        createTxnWrite().flatMap { txn =>
          withTxn(txn) { _ =>
            ReaderT { ctx =>
              capture {
                ctx._keys.clear()
                ctx._waitingContinuations.clear()
                ctx._data.clear()
                ctx._joinMap.clear()
              }
            }
          }
        }

      def collectGarbage(ctx: InMemoryCtx, channelsHash: H): Unit = {
        val data = ctx._data.get(channelsHash).exists(_.nonEmpty)
        if (!data) {
          //we still may have empty list, remove it as well
          ctx._data.remove(channelsHash)
        }

        val waitingContinuation = ctx._waitingContinuations.get(channelsHash).exists(_.nonEmpty)
        if (!waitingContinuation) {
          //we still may have empty list, remove it as well
          ctx._waitingContinuations.remove(channelsHash)
        }

        val channels = ctx._keys.getOrElse(channelsHash, Seq.empty[C])
        val joins    = channels.size == 1 && ctx._joinMap.contains(channels.head)

        if (!data && !waitingContinuation && !joins) {
          ctx._keys.remove(channelsHash)
        }
      }
    }
  }
}

object InMemoryStoreInstance {
  def hashBytes(bs: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(bs)

  def hashString(s: String): Array[Byte] =
    hashBytes(s.getBytes(StandardCharsets.UTF_8))

  /* UGLY HACK FOR TESTING */
  def roundTrip[A <: Serializable](a: A): A = {
    val ser = makeSerializeFromSerializable[A]
    ser.decode(ser.encode(a)).fold(throw _, identity)
  }
}
