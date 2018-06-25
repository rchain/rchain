package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicLong

import coop.rchain.rspace.history.{initialize, Branch, ITrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.util.canonicalize
import coop.rchain.shared.AttemptOps._
import coop.rchain.shared.ByteVectorOps._
import coop.rchain.shared.PathOps._
import coop.rchain.shared.Resources.withResource
import coop.rchain.shared.SeqOps._
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava._
import scodec.Codec
import scodec.bits._

import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.concurrent.SyncVar

/**
  * The main store class.
  *
  * To create an instance, use [[LMDBStore.create]].
  */
class LMDBStore[C, P, A, K] private (
    env: Env[ByteBuffer],
    databasePath: Path,
    _dbGNATs: Dbi[ByteBuffer],
    _dbJoins: Dbi[ByteBuffer],
    _trieUpdateCount: AtomicLong,
    _trieUpdates: SyncVar[Seq[TrieUpdate[C, P, A, K]]],
    val trieStore: ITrieStore[Txn[ByteBuffer], Blake2b256Hash, GNAT[C, P, A, K]],
    val trieBranch: Branch
)(implicit
  codecC: Codec[C],
  codecP: Codec[P],
  codecA: Codec[A],
  codecK: Codec[K])
    extends IStore[C, P, A, K] {

  // Good luck trying to get this to resolve as an implicit
  val joinCodec: Codec[Seq[Seq[C]]] = codecSeq(codecSeq(codecC))

  private[rspace] type T = Txn[ByteBuffer]

  val eventsCounter: StoreEventsCounter = new StoreEventsCounter()

  private[rspace] def createTxnRead(): T = env.txnRead

  private[rspace] def createTxnWrite(): T = env.txnWrite

  private[rspace] def withTxn[R](txn: T)(f: T => R): R =
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

  /* Basic operations */
  private[this] def fetchGNAT(txn: T, channelsHash: Blake2b256Hash): Option[GNAT[C, P, A, K]] = {
    val channelsHashBuff = channelsHash.bytes.toDirectByteBuffer
    Option(_dbGNATs.get(txn, channelsHashBuff)).map { bytes =>
      Codec[GNAT[C, P, A, K]].decode(BitVector(bytes)).map(_.value).get
    }
  }

  private[this] def insertGNAT(txn: T,
                               channelsHash: Blake2b256Hash,
                               gnat: GNAT[C, P, A, K]): Unit = {
    val channelsHashBuff = channelsHash.bytes.toDirectByteBuffer
    val gnatBuff         = Codec[GNAT[C, P, A, K]].encode(gnat).map(_.bytes.toDirectByteBuffer).get
    if (_dbGNATs.put(txn, channelsHashBuff, gnatBuff)) {
      val count   = _trieUpdateCount.getAndIncrement()
      val currLog = _trieUpdates.take()
      _trieUpdates.put(currLog :+ TrieUpdate(count, Insert, channelsHash, gnat))
    } else {
      throw new Exception(s"could not persist: $gnat")
    }
  }

  private def deleteGNAT(txn: Txn[ByteBuffer],
                         channelsHash: Blake2b256Hash,
                         gnat: GNAT[C, P, A, K]): Unit = {
    val channelsHashBuff = channelsHash.bytes.toDirectByteBuffer
    if (_dbGNATs.delete(txn, channelsHashBuff)) {
      val count   = _trieUpdateCount.getAndIncrement()
      val currLog = _trieUpdates.take()
      _trieUpdates.put(currLog :+ TrieUpdate(count, Delete, channelsHash, gnat))
    } else {
      throw new Exception(s"could not delete: $channelsHash")
    }
  }

  private[this] def fetchJoin(txn: T, joinedChannelHash: Blake2b256Hash): Option[Seq[Seq[C]]] = {
    val joinedChannelHashBuff = joinedChannelHash.bytes.toDirectByteBuffer
    Option(_dbJoins.get(txn, joinedChannelHashBuff))
      .map { bytes =>
        joinCodec.decode(BitVector(bytes)).map(_.value).get
      }
  }

  private[this] def insertJoin(txn: T,
                               joinedChannelHash: Blake2b256Hash,
                               joins: Seq[Seq[C]]): Unit = {
    val channelsHashBuff   = joinedChannelHash.bytes.toDirectByteBuffer
    val joinedChannelsBuff = joinCodec.encode(joins).map(_.bytes.toDirectByteBuffer).get
    if (!_dbJoins.put(txn, channelsHashBuff, joinedChannelsBuff)) {
      throw new Exception(s"could not persist: $joins")
    }
  }

  private[rspace] def hashChannels(channels: Seq[C]): Blake2b256Hash =
    Codec[Seq[C]]
      .encode(channels)
      .map((bitVec: BitVector) => Blake2b256Hash.create(bitVec.toByteArray))
      .get

  /* Channels */

  private[rspace] def getChannels(txn: T, channelsHash: Blake2b256Hash): Seq[C] =
    fetchGNAT(txn, channelsHash).map(_.channels).getOrElse(Seq.empty)

  /* Data */

  private[rspace] def putDatum(txn: T, channels: Seq[C], datum: Datum[A]): Unit = {
    val channelsHash = hashChannels(channels)
    fetchGNAT(txn, channelsHash) match {
      case Some(gnat @ GNAT(_, currData, _)) =>
        insertGNAT(txn, channelsHash, gnat.copy(data = datum +: currData))
      case None =>
        insertGNAT(txn, channelsHash, GNAT(channels, Seq(datum), Seq.empty))
    }
  }

  private[rspace] def getData(txn: T, channels: Seq[C]): Seq[Datum[A]] = {
    val channelsHash = hashChannels(channels)
    fetchGNAT(txn, channelsHash).map(_.data).getOrElse(Seq.empty)
  }

  private[rspace] def removeDatum(txn: T, channels: Seq[C], index: Int): Unit = {
    val channelsHash = hashChannels(channels)
    fetchGNAT(txn, channelsHash) match {
      case Some(gnat @ GNAT(_, currData, Seq())) =>
        val newData = dropIndex(currData, index)
        if (newData.nonEmpty)
          insertGNAT(txn, channelsHash, gnat.copy(data = newData))
        else
          deleteGNAT(txn, channelsHash, gnat)
      case Some(gnat @ GNAT(_, currData, _)) =>
        val newData = dropIndex(currData, index)
        insertGNAT(txn, channelsHash, gnat.copy(data = newData))
      case None =>
        throw new Exception("Attempted to remove a datum from a value that doesn't exist")
    }
  }

  private[rspace] def removeDatum(txn: T, channel: C, index: Int): Unit =
    removeDatum(txn, Seq(channel), index)

  /* Continuations */

  private[rspace] def putWaitingContinuation(txn: T,
                                             channels: Seq[C],
                                             continuation: WaitingContinuation[P, K]): Unit = {
    val channelsHash = hashChannels(channels)
    fetchGNAT(txn, channelsHash) match {
      case Some(gnat @ GNAT(_, _, currContinuations)) =>
        insertGNAT(txn, channelsHash, gnat.copy(wks = continuation +: currContinuations))
      case None =>
        insertGNAT(txn, channelsHash, GNAT(channels, Seq.empty, Seq(continuation)))
    }
  }

  private[rspace] def getWaitingContinuation(txn: T,
                                             channels: Seq[C]): Seq[WaitingContinuation[P, K]] = {
    val channelsHash = hashChannels(channels)
    fetchGNAT(txn, channelsHash).map(_.wks).getOrElse(Seq.empty)
  }

  private[rspace] def removeWaitingContinuation(txn: T, channels: Seq[C], index: Int): Unit = {
    val channelsHash = hashChannels(channels)
    fetchGNAT(txn, channelsHash) match {
      case Some(gnat @ GNAT(_, Seq(), currContinuations)) =>
        val newContinuations = dropIndex(currContinuations, index)
        if (newContinuations.nonEmpty)
          insertGNAT(txn, channelsHash, gnat.copy(wks = newContinuations))
        else
          deleteGNAT(txn, channelsHash, gnat)
      case Some(gnat @ GNAT(_, _, currContinuations)) =>
        val newContinuations = dropIndex(currContinuations, index)
        insertGNAT(txn, channelsHash, gnat.copy(wks = newContinuations))
      case None =>
        throw new Exception("Attempted to remove a continuation from a value that doesn't exist")
    }
  }

  private[rspace] def removeAll(txn: Txn[ByteBuffer], channels: Seq[C]): Unit = {
    val channelsHash = hashChannels(channels)
    fetchGNAT(txn, channelsHash).foreach { gnat =>
      insertGNAT(txn, channelsHash, gnat.copy(data = Seq.empty, wks = Seq.empty))
    }
    for (c <- channels) removeJoin(txn, c, channels)
  }

  /* Joins */

  private[rspace] def getJoin(txn: Txn[ByteBuffer], channel: C): Seq[Seq[C]] = {
    val joinedChannelHash = hashChannels(Seq(channel))
    fetchJoin(txn, joinedChannelHash).getOrElse(Seq.empty)
  }

  private[rspace] def addJoin(txn: T, channel: C, channels: Seq[C]): Unit = {
    val joinedChannelHash = hashChannels(Seq(channel))
    fetchJoin(txn, joinedChannelHash) match {
      case Some(joins) if !joins.contains(channels) =>
        insertJoin(txn, joinedChannelHash, channels +: joins)
      case None =>
        insertJoin(txn, joinedChannelHash, Seq(channels))
      case _ =>
        ()
    }
  }

  private[rspace] def removeJoin(txn: T, channel: C, channels: Seq[C]): Unit = {
    val joinedChannelHash = hashChannels(Seq(channel))
    fetchJoin(txn, joinedChannelHash) match {
      case Some(joins) if joins.contains(channels) =>
        if (getWaitingContinuation(txn, channels).isEmpty) {
          val newJoins = removeFirst(joins)(_ == channels)
          if (newJoins.nonEmpty)
            insertJoin(txn, joinedChannelHash, removeFirst(joins)(_ == channels))
          else
            _dbJoins.delete(txn, joinedChannelHash.bytes.toDirectByteBuffer)
        }
      case None =>
        ()
    }
  }

  private[rspace] def joinMap: Map[Blake2b256Hash, Seq[Seq[C]]] =
    withTxn(createTxnRead()) { txn =>
      withResource(_dbJoins.iterate(txn)) { (it: CursorIterator[ByteBuffer]) =>
        it.asScala
          .foldLeft(Map.empty[Blake2b256Hash, Seq[Seq[C]]]) {
            (acc: Map[Blake2b256Hash, Seq[Seq[C]]], x: CursorIterator.KeyVal[ByteBuffer]) =>
              val hash     = Codec[Blake2b256Hash].decode(BitVector(x.key())).map(_.value).get
              val channels = joinCodec.decode(BitVector(x.`val`())).map(_.value).get
              acc.updated(hash, channels)
          }
      }
    }

  private[rspace] def clear(txn: T): Unit = {
    _dbGNATs.drop(txn)
    _dbJoins.drop(txn)
    eventsCounter.reset()
  }

  def close(): Unit = {
    _dbGNATs.close()
    _dbJoins.close()
  }

  def getStoreCounters: StoreCounters =
    eventsCounter.createCounters(databasePath.folderSize, env.stat().entries)

  def isEmpty: Boolean =
    withTxn(createTxnRead()) { txn =>
      !_dbGNATs.iterate(txn).hasNext &&
      !_dbJoins.iterate(txn).hasNext
    }

  def getPatterns(txn: T, channels: Seq[C]): Seq[Seq[P]] =
    getWaitingContinuation(txn, channels).map(_.patterns)

  def toMap: Map[Seq[C], Row[P, A, K]] =
    withTxn(createTxnRead()) { txn =>
      withResource(_dbGNATs.iterate(txn)) { (it: CursorIterator[ByteBuffer]) =>
        it.asScala.map { (x: CursorIterator.KeyVal[ByteBuffer]) =>
          val row  = x.`val`()
          val gnat = Codec[GNAT[C, P, A, K]].decode(BitVector(row)).map(_.value).get
          (gnat.channels, Row(gnat.data, gnat.wks))
        }.toMap
      }
    }

  def createCheckpoint(): Blake2b256Hash = {
    val trieUpdates = _trieUpdates.take
    _trieUpdates.put(Seq.empty)
    _trieUpdateCount.set(0L)
    collapse(trieUpdates).foreach {
      case TrieUpdate(_, Insert, channelsHash, gnat) =>
        history.insert(trieStore, trieBranch, channelsHash, canonicalize(gnat))
      case TrieUpdate(_, Delete, channelsHash, gnat) =>
        history.delete(trieStore, trieBranch, channelsHash, canonicalize(gnat))
    }
    withTxn(createTxnRead()) { txn =>
      trieStore.getRoot(txn, trieBranch).getOrElse(throw new Exception("Could not get root hash"))
    }
  }

  // TODO: Does using a cursor improve performance for bulk operations?
  private[rspace] def bulkInsert(txn: Txn[ByteBuffer],
                                 gnats: Seq[(Blake2b256Hash, GNAT[C, P, A, K])]): Unit =
    gnats.foreach {
      case (hash, gnat @ GNAT(channels, _, wks)) =>
        insertGNAT(txn, hash, gnat)
        for {
          wk      <- wks
          channel <- channels
        } {
          addJoin(txn, channel, channels)
        }
    }
}

object LMDBStore {

  def create[C, P, A, K](context: Context[C, P, A, K], branch: Branch)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]): LMDBStore[C, P, A, K] = {
    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val dbGnats: Dbi[ByteBuffer] = context.env.openDbi(s"${branch.name}-gnats", MDB_CREATE)
    val dbJoins: Dbi[ByteBuffer] = context.env.openDbi(s"${branch.name}-joins", MDB_CREATE)

    val trieUpdateCount = new AtomicLong(0L)
    val trieUpdates     = new SyncVar[Seq[TrieUpdate[C, P, A, K]]]()
    trieUpdates.put(Seq.empty)

    new LMDBStore[C, P, A, K](context.env,
                              context.path,
                              dbGnats,
                              dbJoins,
                              trieUpdateCount,
                              trieUpdates,
                              context.trieStore,
                              branch)
  }

  def create[C, P, A, K](path: Path, mapSize: Long, noTls: Boolean = true)(
      implicit sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]): LMDBStore[C, P, A, K] = {
    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val flags =
      if (noTls)
        List(EnvFlags.MDB_NOTLS)
      else
        List.empty[EnvFlags]

    val env    = Context.create[C, P, A, K](path, mapSize, flags)
    val branch = Branch.MASTER

    initialize(env.trieStore, branch)

    create(env, branch)
  }
}
