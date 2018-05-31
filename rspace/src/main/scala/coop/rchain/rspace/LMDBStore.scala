package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicLong

import coop.rchain.rspace.history.{initialize, Blake2b256Hash, LMDBTrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.util._
import coop.rchain.shared.AttemptOps._
import coop.rchain.shared.ByteVectorOps._
import coop.rchain.shared.PathOps._
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
    _trieStore: LMDBTrieStore[Blake2b256Hash, GNAT[C, P, A, K]]
)(implicit
  codecC: Codec[C],
  codecP: Codec[P],
  codecA: Codec[A],
  codecK: Codec[K])
    extends IStore[C, P, A, K]
    with ITestableStore[C, P] {

  // Good luck trying to get this to resolve as an implicit
  val joinCodec: Codec[Seq[Seq[C]]] = codecSeq(codecSeq(codecC))

  private[rspace] type H = Blake2b256Hash

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

  private[this] def fetchGNAT(txn: T, channelsHash: H): Option[GNAT[C, P, A, K]] = {
    val channelsHashBuff = channelsHash.bytes.toDirectByteBuffer
    Option(_dbGNATs.get(txn, channelsHashBuff)).map { bytes =>
      Codec[GNAT[C, P, A, K]].decode(BitVector(bytes)).map(_.value).get
    }
  }

  private[this] def insertGNAT(txn: T, channelsHash: H, gnat: GNAT[C, P, A, K]): Unit = {
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

  private[this] def fetchJoin(txn: T, joinedChannelHash: H): Option[Seq[Seq[C]]] = {
    val joinedChannelHashBuff = joinedChannelHash.bytes.toDirectByteBuffer
    Option(_dbJoins.get(txn, joinedChannelHashBuff))
      .map { bytes =>
        joinCodec.decode(BitVector(bytes)).map(_.value).get
      }
  }

  private[this] def insertJoin(txn: T, joinedChannelHash: H, joins: Seq[Seq[C]]): Unit = {
    val channelsHashBuff   = joinedChannelHash.bytes.toDirectByteBuffer
    val joinedChannelsBuff = joinCodec.encode(joins).map(_.bytes.toDirectByteBuffer).get
    if (!_dbJoins.put(txn, channelsHashBuff, joinedChannelsBuff)) {
      throw new Exception(s"could not persist: $joins")
    }
  }

  private[rspace] def hashChannels(channels: Seq[C]): H =
    Codec[Seq[C]]
      .encode(channels)
      .map((bitVec: BitVector) => Blake2b256Hash.create(bitVec.toByteArray))
      .get

  /* Channels */

  private[rspace] def getChannels(txn: T, channelsHash: H): Seq[C] =
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

  private[rspace] def removeAllJoins(txn: T, channel: C): Unit = {
    val joinedChannelHash = hashChannels(Seq(channel))
    _dbJoins.delete(txn, joinedChannelHash.bytes.toDirectByteBuffer)
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
            removeAllJoins(txn, channel)
        }
      case None =>
        ()
    }
  }

  private[rspace] def clear(): Unit = {
    withTxn(createTxnWrite()) { txn =>
      _dbGNATs.drop(txn)
      _dbJoins.drop(txn)
    }
    eventsCounter.reset()
  }

  def close(): Unit = {
    _dbGNATs.close()
    _dbJoins.close()
    env.close()
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

  def getCheckpoint(): Blake2b256Hash = {
    val trieUpdates = _trieUpdates.take
    _trieUpdates.put(Seq.empty)
    _trieUpdateCount.set(0L)
    trieUpdates.foreach {
      case TrieUpdate(_, Insert, channelsHash, gnat) =>
        history.insert(_trieStore, channelsHash, gnat)
      case TrieUpdate(_, Delete, channelsHash, gnat) =>
        history.delete(_trieStore, channelsHash, gnat)
    }
    withTxn(createTxnRead()) { txn =>
      _trieStore.getRoot(txn).getOrElse(throw new Exception("Could not get root hash"))
    }
  }
}

object LMDBStore {
  private[this] val dataTableName: String  = "GNATs"
  private[this] val joinsTableName: String = "Joins"

  /**
    * Creates an instance of [[LMDBStore]]
    *
    * @param path    Path to the database files
    * @param mapSize Maximum size of the database, in bytes
    * @tparam C A type representing a channel
    * @tparam P A type representing a pattern
    * @tparam A A type representing a piece of data
    * @tparam K A type representing a continuation
    */
  def create[C, P, A, K](path: Path, mapSize: Long)(implicit sc: Serialize[C],
                                                    sp: Serialize[P],
                                                    sa: Serialize[A],
                                                    sk: Serialize[K]): LMDBStore[C, P, A, K] = {

    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val env: Env[ByteBuffer] =
      Env
        .create()
        .setMapSize(mapSize)
        .setMaxDbs(8)
        .setMaxReaders(126)
        .open(path.toFile)

    val dbGNATs: Dbi[ByteBuffer] = env.openDbi(dataTableName, MDB_CREATE)
    val dbJoins: Dbi[ByteBuffer] = env.openDbi(joinsTableName, MDB_CREATE)

    val trieUpdateCount = new AtomicLong(0L)
    val trieUpdates     = new SyncVar[Seq[TrieUpdate[C, P, A, K]]]()
    trieUpdates.put(Seq.empty)

    val trieStore = LMDBTrieStore.create[Blake2b256Hash, GNAT[C, P, A, K]](env)

    if (history.getRoot(trieStore).isEmpty) {
      initialize(trieStore)
    }

    new LMDBStore[C, P, A, K](env, path, dbGNATs, dbJoins, trieUpdateCount, trieUpdates, trieStore)
  }
}
