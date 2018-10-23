package coop.rchain.rspace

import java.nio.ByteBuffer
import java.nio.file.Path

import internal._
import coop.rchain.rspace.history.{Branch, ITrieStore}
import coop.rchain.rspace.internal._
import coop.rchain.rspace.util.canonicalize
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

/**
  * The main store class.
  *
  * To create an instance, use [[LMDBStore.create]].
  */
class LMDBStore[C, P, A, K] private[rspace] (
    val env: Env[ByteBuffer],
    protected[this] val databasePath: Path,
    private[this] val _dbGNATs: Dbi[ByteBuffer],
    private[this] val _dbJoins: Dbi[ByteBuffer],
    val trieStore: ITrieStore[Txn[ByteBuffer], Blake2b256Hash, GNAT[C, P, A, K]],
    val trieBranch: Branch
)(
    implicit
    codecC: Codec[C],
    codecP: Codec[P],
    codecA: Codec[A],
    codecK: Codec[K]
) extends IStore[C, P, A, K]
    with LMDBOps {

  // Good luck trying to get this to resolve as an implicit
  val joinCodec: Codec[Seq[Seq[C]]] = codecSeq(codecSeq(codecC))

  private[rspace] type TrieTransaction = Transaction

  def withTrieTxn[R](txn: Transaction)(f: TrieTransaction => R): R = f(txn)

  /* Basic operations */
  private[this] def fetchGNAT(
      txn: Transaction,
      channelsHash: Blake2b256Hash
  ): Option[GNAT[C, P, A, K]] =
    _dbGNATs.get(txn, channelsHash)(codecGNAT[C, P, A, K])

  private[this] def insertGNAT(
      txn: Transaction,
      channelsHash: Blake2b256Hash,
      gnat: GNAT[C, P, A, K]
  ): Unit = {
    _dbGNATs.put(txn, channelsHash, gnat)
    trieInsert(channelsHash, gnat)
  }

  private def deleteGNAT(
      txn: Txn[ByteBuffer],
      channelsHash: Blake2b256Hash,
      gnat: GNAT[C, P, A, K]
  ): Unit = {
    _dbGNATs.delete(txn, channelsHash)
    trieDelete(channelsHash, gnat)
  }

  private[this] def fetchJoin(
      txn: Transaction,
      joinedChannelHash: Blake2b256Hash
  ): Option[Seq[Seq[C]]] =
    _dbJoins.get(txn, joinedChannelHash)(joinCodec)

  private[this] def insertJoin(
      txn: Transaction,
      joinedChannelHash: Blake2b256Hash,
      joins: Seq[Seq[C]]
  ): Unit =
    _dbJoins.put(txn, joinedChannelHash, joins)(joinCodec)

  private[rspace] def hashChannels(channels: Seq[C]): Blake2b256Hash =
    StableHashProvider.hash(channels)

  /* Channels */

  private[rspace] def getChannels(txn: Transaction, channelsHash: Blake2b256Hash): Seq[C] =
    fetchGNAT(txn, channelsHash).map(_.channels).getOrElse(Seq.empty)

  /* Data */

  private[rspace] def putDatum(txn: Transaction, channels: Seq[C], datum: Datum[A]): Unit = {
    val channelsHash = hashChannels(channels)
    fetchGNAT(txn, channelsHash) match {
      case Some(gnat @ GNAT(_, currData, _)) =>
        insertGNAT(txn, channelsHash, gnat.copy(data = datum +: currData))
      case None =>
        insertGNAT(txn, channelsHash, GNAT(channels, Seq(datum), Seq.empty))
    }
  }

  private[rspace] def getData(txn: Transaction, channels: Seq[C]): Seq[Datum[A]] = {
    val channelsHash = hashChannels(channels)
    fetchGNAT(txn, channelsHash).map(_.data).getOrElse(Seq.empty)
  }

  private[rspace] def removeDatum(txn: Transaction, channels: Seq[C], index: Int): Unit = {
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

  private[rspace] def removeDatum(txn: Transaction, channel: C, index: Int): Unit =
    removeDatum(txn, Seq(channel), index)

  /* Continuations */

  private[rspace] def installWaitingContinuation(
      txn: Transaction,
      channels: Seq[C],
      continuation: WaitingContinuation[P, K]
  ): Unit =
    _dbGNATs.put(
      txn,
      hashChannels(channels),
      GNAT[C, P, A, K](channels, Seq.empty, Seq(continuation))
    )

  private[rspace] def putWaitingContinuation(
      txn: Transaction,
      channels: Seq[C],
      continuation: WaitingContinuation[P, K]
  ): Unit = {
    val channelsHash = hashChannels(channels)
    fetchGNAT(txn, channelsHash) match {
      case Some(gnat @ GNAT(_, _, currContinuations)) =>
        insertGNAT(txn, channelsHash, gnat.copy(wks = continuation +: currContinuations))
      case None =>
        insertGNAT(txn, channelsHash, GNAT(channels, Seq.empty, Seq(continuation)))
    }
  }

  private[rspace] def getWaitingContinuation(
      txn: Transaction,
      channels: Seq[C]
  ): Seq[WaitingContinuation[P, K]] = {
    val channelsHash = hashChannels(channels)
    fetchGNAT(txn, channelsHash).map(_.wks).getOrElse(Seq.empty)
  }

  private[rspace] def removeWaitingContinuation(
      txn: Transaction,
      channels: Seq[C],
      index: Int
  ): Unit = {
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

  /* Joins */

  private[rspace] def getJoin(txn: Txn[ByteBuffer], channel: C): Seq[Seq[C]] = {
    val joinedChannelHash = hashChannels(Seq(channel))
    fetchJoin(txn, joinedChannelHash).getOrElse(Seq.empty)
  }

  private[rspace] def addJoin(txn: Transaction, channel: C, channels: Seq[C]): Unit = {
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

  private[rspace] def removeJoin(txn: Transaction, channel: C, channels: Seq[C]): Unit = {
    val joinedChannelHash = hashChannels(Seq(channel))
    fetchJoin(txn, joinedChannelHash) match {
      case Some(joins) if joins.contains(channels) =>
        if (getWaitingContinuation(txn, channels).isEmpty) {
          val newJoins = removeFirst(joins)(_ == channels)
          if (newJoins.nonEmpty)
            insertJoin(txn, joinedChannelHash, removeFirst(joins)(_ == channels))
          else
            _dbJoins.delete(txn, joinedChannelHash)
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

  private[rspace] def clear(txn: Transaction): Unit = {
    _dbGNATs.drop(txn)
    _dbJoins.drop(txn)
  }

  override def close(): Unit = {
    super.close()
    _dbGNATs.close()
    _dbJoins.close()
  }

  def isEmpty: Boolean =
    withTxn(createTxnRead()) { txn =>
      !_dbGNATs.iterate(txn).hasNext &&
      !_dbJoins.iterate(txn).hasNext
    }

  def getPatterns(txn: Transaction, channels: Seq[C]): Seq[Seq[P]] =
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

  protected def processTrieUpdate(update: TrieUpdate[C, P, A, K]): Unit =
    update match {
      case TrieUpdate(_, Insert, channelsHash, gnat) =>
        history.insert(trieStore, trieBranch, channelsHash, canonicalize(gnat))
      case TrieUpdate(_, Delete, channelsHash, gnat) =>
        history.delete(trieStore, trieBranch, channelsHash, canonicalize(gnat))
    }

  // TODO: Does using a cursor improve performance for bulk operations?
  private[rspace] def bulkInsert(
      txn: Txn[ByteBuffer],
      gnats: Seq[(Blake2b256Hash, GNAT[C, P, A, K])]
  ): Unit =
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

  def create[C, P, A, K](context: LMDBContext[C, P, A, K], branch: Branch = Branch.MASTER)(
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]
  ): LMDBStore[C, P, A, K] = {
    implicit val codecC: Codec[C] = sc.toCodec
    implicit val codecP: Codec[P] = sp.toCodec
    implicit val codecA: Codec[A] = sa.toCodec
    implicit val codecK: Codec[K] = sk.toCodec

    val dbGnats: Dbi[ByteBuffer] = context.env.openDbi(s"${branch.name}-gnats", MDB_CREATE)
    val dbJoins: Dbi[ByteBuffer] = context.env.openDbi(s"${branch.name}-joins", MDB_CREATE)

    new LMDBStore[C, P, A, K](
      context.env,
      context.path,
      dbGnats,
      dbJoins,
      context.trieStore,
      branch
    )
  }
}
