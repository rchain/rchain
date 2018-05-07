package coop.rchain.rspace.pure

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.concurrent.locks.StampedLock
import javax.xml.bind.DatatypeConverter.printHexBinary

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

  implicit def storeInMemory[F[_]: Monad, C, P, A, K <: Serializable](
      implicit
      serializeC: Serialize[C],
      captureF: Capture[F]
  ): Store[ReaderT[F, LMDBContext, ?], C, P, A, K] =
    new Store[ReaderT[F, LMDBContext, ?], C, P, A, K] {
      private[this] def capture[X](x: X): F[X] = captureF.capture(x)

      private[this] val lock = new StampedLock()

      private[this] val _keys = mutable.HashMap.empty[String, Seq[C]]
      private[this] val _waitingContinuations =
        mutable.HashMap.empty[String, Seq[WaitingContinuation[P, K]]]
      private[this] val _data = mutable.HashMap.empty[String, Seq[Datum[A]]]
      private[this] val _joinMap = new mutable.HashMap[C, mutable.Set[String]]
      with mutable.MultiMap[C, String]

      override type H = String

      override type T = Long

      override def createTxnRead(): ReaderT[F, LMDBContext, T] =
        ReaderT(_ => capture(lock.readLock()))

      override def createTxnWrite(): ReaderT[F, LMDBContext, T] =
        ReaderT(_ => capture(lock.writeLock()))

      override def withTxn[R](txn: T)(f: T => R): R =
        try {
          f(txn)
        } finally {
          lock.unlock(txn)
        }

      private[this] def hashChannelsInner(channels: Seq[C]): H =
        printHexBinary(hashBytes(channels.flatMap(serializeC.encode).toArray))

      override def hashChannels(channels: Seq[C]): ReaderT[F, LMDBContext, H] =
        ReaderT.pure(hashChannelsInner(channels))

      override def getChannels(txn: T, channelsHash: H): ReaderT[F, LMDBContext, Seq[C]] =
        ReaderT(_ => capture(_keys.getOrElse(channelsHash, Seq.empty[C])))

      private[rspace] def putChannels(txn: T, channels: Seq[C]): ReaderT[F, LMDBContext, Unit] =
        ReaderT(_ => capture(_keys.update(hashChannelsInner(channels), channels)))

      override def getData(txn: T, channels: Seq[C]): ReaderT[F, LMDBContext, Seq[Datum[A]]] =
        ReaderT(_ => capture(_data.getOrElse(hashChannelsInner(channels), Seq.empty[Datum[A]])))

      override def putDatum(txn: T,
                            channels: Seq[C],
                            datum: Datum[A]): ReaderT[F, LMDBContext, Unit] =
        ReaderT(_ =>
          capture {
            val channelsHash = hashChannelsInner(channels)
            putChannels(txn, channels)
            val datums = _data.getOrElseUpdate(channelsHash, Seq.empty[Datum[A]])
            _data.update(channelsHash, datum +: datums)
        })

      override def removeDatum(txn: T, channel: C, index: Int): ReaderT[F, LMDBContext, Unit] =
        removeDatum(txn, Seq(channel), index)

      override def removeDatum(txn: T,
                               channels: Seq[C],
                               index: Int): ReaderT[F, LMDBContext, Unit] =
        ReaderT(_ =>
          capture {
            val channelsHash = hashChannelsInner(channels)
            for (data <- _data.get(channelsHash)) {
              _data.update(channelsHash, dropIndex(data, index))
            }
            collectGarbage(channelsHash)
        })

      override def getWaitingContinuation(
          txn: T,
          channels: Seq[C]): ReaderT[F, LMDBContext, Seq[WaitingContinuation[P, K]]] =
        ReaderT(_ =>
          capture {
            _waitingContinuations
              .getOrElse(hashChannelsInner(channels), Seq.empty[WaitingContinuation[P, K]])
              .map { (wk: WaitingContinuation[P, K]) =>
                wk.copy(continuation = InMemoryStoreInstance.roundTrip(wk.continuation))
              }
        })

      override def removeWaitingContinuation(txn: T,
                                             channels: Seq[C],
                                             index: Int): ReaderT[F, LMDBContext, Unit] =
        ReaderT(_ =>
          capture {
            val channelsHash = hashChannelsInner(channels)
            for (waitingContinuation <- _waitingContinuations.get(channelsHash)) {
              _waitingContinuations.update(channelsHash, dropIndex(waitingContinuation, index))
            }
            collectGarbage(channelsHash)
        })

      override def putWaitingContinuation(
          txn: T,
          channels: Seq[C],
          continuation: WaitingContinuation[P, K]): ReaderT[F, LMDBContext, Unit] =
        ReaderT(_ =>
          capture {
            val channelsHash = hashChannelsInner(channels)
            putChannels(txn, channels)
            val waitingContinuations =
              _waitingContinuations.getOrElseUpdate(channelsHash,
                                                    Seq.empty[WaitingContinuation[P, K]])
            _waitingContinuations.update(channelsHash, waitingContinuations :+ continuation)
        })

      override def removeJoin(txn: T, channel: C, channels: Seq[C]): ReaderT[F, LMDBContext, Unit] =
        ReaderT(_ =>
          capture {
            val joinKey      = hashChannelsInner(Seq(channel))
            val channelsHash = hashChannelsInner(channels)
            if (_waitingContinuations.get(channelsHash).forall(_.isEmpty)) {
              _joinMap.removeBinding(channel, channelsHash)
            }
            collectGarbage(joinKey)
        })

      override def removeAllJoins(txn: T, channel: C): ReaderT[F, LMDBContext, Unit] =
        ReaderT(_ =>
          capture {
            _joinMap.remove(channel)
            collectGarbage(hashChannelsInner(Seq(channel)))
        })

      override def putJoin(txn: T, channel: C, channels: Seq[C]): ReaderT[F, LMDBContext, Unit] =
        ReaderT(_ => capture(_joinMap.addBinding(channel, hashChannelsInner(channels))))

      override def getJoin(txn: T, channel: C): ReaderT[F, LMDBContext, Seq[Seq[C]]] =
        ReaderT(
          _ =>
            capture(
              _joinMap
                .getOrElse(channel, Set.empty[String])
                .toList
                .map(_keys.getOrElse(_, Seq.empty[C]))))

      override def removeAll(txn: T, channels: Seq[C]): ReaderT[F, LMDBContext, Unit] =
        ReaderT(_ =>
          capture {
            val channelsHash = hashChannelsInner(channels)
            _data.put(channelsHash, Seq.empty)
            _waitingContinuations.put(channelsHash, Seq.empty)
            for (c <- channels) removeJoin(txn, c, channels)
        })

      override def toMap: ReaderT[F, LMDBContext, Map[Seq[C], Row[P, A, K]]] =
        ReaderT(_ =>
          capture {
            _keys.map {
              case (channelsHash, channels) =>
                val data = _data.getOrElse(channelsHash, Seq.empty[Datum[A]])
                val waitingContinuations =
                  _waitingContinuations.getOrElse(channelsHash,
                                                  Seq.empty[WaitingContinuation[P, K]])
                (channels, Row(data, waitingContinuations))
            }.toMap
        })

      override def close(): ReaderT[F, LMDBContext, Unit] = ReaderT.pure(())

      private[rspace] def clear(): ReaderT[F, LMDBContext, Unit] =
        createTxnWrite().flatMap(txn =>
          withTxn(txn) { _ =>
            ReaderT(_ =>
              capture {
                _keys.clear()
                _waitingContinuations.clear()
                _data.clear()
                _joinMap.clear()
                ()
            })
        })

      def collectGarbage(channelsHash: H): Unit = {
        val data = _data.get(channelsHash).exists(_.nonEmpty)
        if (!data) {
          //we still may have empty list, remove it as well
          _data.remove(channelsHash)
        }

        val waitingContinuation = _waitingContinuations.get(channelsHash).exists(_.nonEmpty)
        if (!waitingContinuation) {
          //we still may have empty list, remove it as well
          _waitingContinuations.remove(channelsHash)
        }

        val channels = _keys.getOrElse(channelsHash, Seq.empty[C])
        val joins    = channels.size == 1 && _joinMap.contains(channels.head)

        if (!data && !waitingContinuation && !joins) {
          _keys.remove(channelsHash)
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
