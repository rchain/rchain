package coop.rchain.rspace

import java.nio.ByteBuffer
import java.security.MessageDigest

import cats.implicits._
import com.google.protobuf.ByteString
import com.trueaccord.scalapb.GeneratedMessage
import coop.rchain.rspace.Serialize.mkProtobufInstance
import coop.rchain.rspace.datamodels._
import coop.rchain.rspace.internal.{Datum, Row, WaitingContinuation}
import coop.rchain.rspace.util.{dropIndex, ignore, optionalWithResource}

trait KeyValueAmbry {
  private[rspace] type H = ByteBuffer

  private[rspace] type T

  trait KeyValueTable {
    def iterateKeys(txn: T): Iterator[H]
    def put(txn: T, keyBuffer: H, valueBuffer: ByteBuffer): Unit
    def get(txn: T, keyBuffer: H): Option[ByteBuffer]
    def delete(txn: T, keyBuffer: H): Unit
    def isEmpty(txn: T): Boolean
    def drop(txn: T): Unit
  }

  val _dbKeys: KeyValueTable
  val _dbAs: KeyValueTable
  val _dbPsKs: KeyValueTable
  val _dbJoins: KeyValueTable
}

/**
	* The main store class.
	*
	* To create an instance, use [[LMDBStore.create]].
	*/
abstract class Store[C, P, A, K]()(implicit
                                   sc: Serialize[C],
                                   sp: Serialize[P],
                                   sa: Serialize[A],
                                   sk: Serialize[K],
                                   sbl: Serialize[BytesList])
    extends KeyValueAmbry
    with IStore[C, P, A, K]
    with ITestableStore[C, P] {

  import coop.rchain.rspace.Store._

  private[rspace] def hashCs(cs: List[C])(implicit st: Serialize[C]): H =
    toByteBuffer(hashBytes(toBytesList(cs)(st).toByteArray))

  private[rspace] def getKey(txn: T, s: H): List[C] =
    _dbKeys.get(txn, s).map(fromByteBuffer[C]).getOrElse(List.empty[C])

  private[rspace] def putCsH(txn: T, channels: List[C]): H = {
    val packedCs = toByteBuffer(toBytesList(channels))
    val keyCs    = toByteBuffer(hashBytes(packedCs))
    _dbKeys.put(txn, keyCs, packedCs)
    keyCs
  }

  private[this] def readAsBytesList(txn: T, keyCs: H): Option[List[AsBytes]] =
    _dbAs
      .get(txn, keyCs)
      .map(bytes => AsBytesList.parseFrom(toArray(bytes)).values.toList)

  private[this] def writeAsBytesList(txn: T, keyCs: H, values: List[AsBytes]): Unit =
    if (values.nonEmpty) {
      _dbAs.put(txn, keyCs, toByteBuffer(AsBytesList().withValues(values)))
    } else {
      _dbAs.delete(txn, keyCs)
      collectGarbage(txn, keyCs, asCollected = true)
    }

  private[rspace] def putA(txn: T, channels: List[C], datum: Datum[A]): Unit = {
    val keyCs   = putCsH(txn, channels)
    val binAs   = AsBytes().withAvalue(toByteString(datum.a)).withPersist(datum.persist)
    val asksLst = readAsBytesList(txn, keyCs).getOrElse(List.empty[AsBytes])
    writeAsBytesList(txn, keyCs, binAs :: asksLst)
  }

  private[rspace] def getAs(txn: T, channels: List[C]): List[Datum[A]] = {
    val keyCs = hashCs(channels)
    readAsBytesList(txn, keyCs)
      .map(_.map(bytes => Datum(fromByteString[A](bytes.avalue), bytes.persist)))
      .getOrElse(List.empty[Datum[A]])
  }

  def collectGarbage(txn: T,
                     keyCs: H,
                     asCollected: Boolean = false,
                     psksCollected: Boolean = false,
                     joinsCollected: Boolean = false): Unit = {

    def isEmpty(kvTable: KeyValueTable): Boolean = kvTable.get(txn, keyCs).isEmpty

    val readyToCollect = (asCollected || isEmpty(_dbAs)) &&
      (psksCollected || isEmpty(_dbPsKs)) &&
      (joinsCollected || isEmpty(_dbJoins))

    if (readyToCollect) {
      _dbKeys.delete(txn, keyCs)
    }
  }

  private[rspace] def removeA(txn: T, channel: C, index: Int): Unit =
    removeA(txn, List(channel), index)

  private[rspace] def removeA(txn: T, channels: List[C], index: Int): Unit = {
    val keyCs = hashCs(channels)
    readAsBytesList(txn, keyCs) match {
      case Some(as) => writeAsBytesList(txn, keyCs, util.dropIndex(as, index))
      case None     => throw new IllegalArgumentException(s"removeA: no values at $channels")
    }
  }

  private[this] def readPsKsBytesList(txn: T, keyCs: H): Option[List[PsKsBytes]] =
    _dbPsKs
      .get(txn, keyCs)
      .map(bytes => PsKsBytesList.parseFrom(toArray(bytes)).values.toList)

  private[this] def writePsKsBytesList(txn: T, keyCs: H, values: List[PsKsBytes]): Unit =
    if (values.nonEmpty) {
      _dbPsKs.put(txn, keyCs, toByteBuffer(PsKsBytesList().withValues(values)))
    } else {
      _dbPsKs.delete(txn, keyCs)
      collectGarbage(txn, keyCs, psksCollected = true)
    }

  private[rspace] def putK(txn: T,
                           channels: List[C],
                           continuation: WaitingContinuation[P, K]): Unit = {
    val keyCs = putCsH(txn, channels)
    val binPsKs =
      PsKsBytes()
        .withKvalue(toByteString(continuation.continuation))
        .withPatterns(toBytesList(continuation.patterns))
        .withPersist(continuation.persist)
    val psksLst = readPsKsBytesList(txn, keyCs).getOrElse(List.empty[PsKsBytes])
    writePsKsBytesList(txn, keyCs, binPsKs +: psksLst)
  }

  private[rspace] def getPsK(txn: T, curr: List[C]): List[WaitingContinuation[P, K]] = {
    val keyCs = hashCs(curr)
    readPsKsBytesList(txn, keyCs)
      .flatMap(_.map(psks =>
        psks.patterns.map(bl =>
          WaitingContinuation(fromBytesList[P](bl), fromByteString[K](psks.kvalue), psks.persist)))
        .sequence[Option, WaitingContinuation[P, K]])
      .getOrElse(List.empty[WaitingContinuation[P, K]])
  }

  private[rspace] def removePsK(txn: T, channels: List[C], index: Int): Unit = {
    val keyCs = hashCs(channels)
    readPsKsBytesList(txn, keyCs) match {
      case Some(psks) => writePsKsBytesList(txn, keyCs, util.dropIndex(psks, index))
      case None       => throw new IllegalArgumentException(s"removePsK: no values at $channels")
    }
  }

  private[rspace] def addJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashCs(List(c))
    val oldCsList =
      _dbJoins
        .get(txn, joinKey)
        .map(fromByteBuffer[BytesList])
        .getOrElse(List.empty[BytesList])

    val addBl = toBytesList(cs)
    if (!oldCsList.contains(addBl)) {
      _dbJoins.put(txn, joinKey, toByteBuffer(toBytesList(addBl :: oldCsList)))
    }
  }

  private[rspace] def getJoin(txn: T, c: C): List[List[C]] = {
    val joinKey = hashCs(List(c))
    _dbJoins
      .get(txn, joinKey)
      .map(fromByteBuffer[BytesList])
      .map(_.map(fromBytesList[C]))
      .getOrElse(List.empty[List[C]])
  }

  private[rspace] def removeJoin(txn: T, c: C, cs: List[C]): Unit = {
    val joinKey = hashCs(List(c))
    val exList =
      _dbJoins
        .get(txn, joinKey)
        .map(fromByteBuffer[BytesList])
        .map(_.map(fromBytesList[C]))
        .getOrElse(List.empty[List[C]])
    val idx = exList.indexOf(cs)
    if (idx >= 0) {
      val csKey = hashCs(cs)
      if (_dbPsKs.get(txn, csKey).isEmpty) {
        val resList = dropIndex(exList, idx)
        if (resList.nonEmpty) {
          _dbJoins.put(txn, joinKey, toByteBuffer(toBytesList(resList.map(toBytesList(_)))))
        } else {
          _dbJoins.delete(txn, joinKey)
          collectGarbage(txn, joinKey, joinsCollected = true)
        }
      }
    } else {
      throw new IllegalArgumentException(s"removeJoin: $cs is not a member of $exList")
    }
  }

  private[rspace] def removeAllJoins(txn: T, c: C): Unit = {
    val joinKey = hashCs(List(c))
    _dbJoins.delete(txn, joinKey)
    collectGarbage(txn, joinKey)
  }

  def isEmpty: Boolean =
    withTxn(createTxnRead()) { txn =>
      _dbKeys.isEmpty(txn) &&
      _dbAs.isEmpty(txn) &&
      _dbPsKs.isEmpty(txn) &&
      _dbJoins.isEmpty(txn)
    }

  def getPs(txn: T, channels: List[C]): List[List[P]] =
    getPsK(txn, channels).map(_.patterns)

  def toMap: Map[List[C], Row[P, A, K]] =
    withTxn(createTxnRead()) { txn =>
      optionalWithResource(_dbKeys.iterateKeys(txn)) { it =>
        it.map(channelKey => {
          val channels: List[C] = getKey(txn, channelKey)
          val data              = getAs(txn, channels)
          val wks               = getPsK(txn, channels)
          (channels, Row(data, wks))
        })
      }.toMap
    }

  private[rspace] def clear(): Unit =
    withTxn(createTxnWrite()) { txn =>
      _dbKeys.drop(txn)
      _dbAs.drop(txn)
      _dbPsKs.drop(txn)
      _dbJoins.drop(txn)
    }

  /**
    * LMDBStore accepts only allocateDirect'ed buffers, while InMemoryStore
    * works fine with zero-copy wrappers
    */
  protected[Store] def toByteBuffer(bytes: Array[Byte]): H

  final def toByteBuffer(message: GeneratedMessage): H = toByteBuffer(message.toByteArray)
}

object Store {
  val bytesListInstance: Serialize[BytesList] = mkProtobufInstance(BytesList)

  private[rspace] def toByteString[T](value: T)(implicit st: Serialize[T]): ByteString =
    ByteString.copyFrom(st.encode(value))

  private[rspace] def fromByteString[T](bytes: ByteString)(implicit st: Serialize[T]): T =
    st.decode(bytes.toByteArray) match {
      case Left(err)    => throw new Exception(err)
      case Right(value) => value
    }

  private[rspace] def toBytesList[T](values: List[T])(implicit st: Serialize[T]): BytesList =
    BytesList().withValues(values.map(st.encode).map(ByteString.copyFrom))

  private[rspace] def fromBytesList[T](bl: BytesList)(implicit st: Serialize[T]): List[T] = {
    val x: Either[Throwable, List[T]] = bl.values
      .map(x => st.decode(x.toByteArray))
      .toList
      .sequence[Either[Throwable, ?], T]
    x match {
      case Left(err)     => throw new Exception(err)
      case Right(values) => values
    }
  }

  private[rspace] def fromByteBuffer[T](byteBuffer: ByteBuffer)(
      implicit st: Serialize[T]): List[T] =
    fromBytesList[T](BytesList.parseFrom(toArray(byteBuffer)))

  private[rspace] def hashBytes(byteBuffer: ByteBuffer): Array[Byte] =
    hashBytes(toArray(byteBuffer))

  private[rspace] def toArray(byteBuffer: ByteBuffer): Array[Byte] = {
    val fetched = new Array[Byte](byteBuffer.remaining())
    byteBuffer.mark()
    ignore {
      byteBuffer.get(fetched)
    }
    byteBuffer.reset()
    fetched
  }

  private[rspace] def hashBytes(bytes: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(bytes)
}
