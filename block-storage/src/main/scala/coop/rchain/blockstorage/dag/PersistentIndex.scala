package coop.rchain.blockstorage.dag

import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}
import java.nio.{BufferUnderflowException, ByteBuffer}

import cats.Monad
import cats.effect.Sync
import cats.implicits._
import cats.mtl.MonadState
import coop.rchain.blockstorage.StorageError
import coop.rchain.blockstorage.dag.PersistentIndex.EncodeError
import coop.rchain.blockstorage.util.Crc32
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io._
import coop.rchain.shared.{AtomicMonadState, Log}
import monix.execution.atomic.AtomicAny
import scodec.Attempt.{Failure, Successful}
import scodec._
import scodec.bits._
import scodec.codecs._

class PersistentIndex[F[_]: Sync: Log: RaiseIOError, K: Codec, V: Codec](
    private val logFilePath: Path,
    private val crcFilePath: Path,
    private val persistedDataState: MonadState[F, Map[K, V]],
    private val crc: Crc32[F],
    private val keyValueCodec: Codec[(K, V)],
    private val outputStream: FileOutputStreamIO[F]
) {
  private def updateCrcFile: F[Unit] =
    for {
      newCrcBytes <- crc.bytes
      tmpCrc      <- createSameDirectoryTemporaryFile(crcFilePath)
      _           <- writeToFile[F](tmpCrc, newCrcBytes)
      _           <- replaceFile(tmpCrc, crcFilePath)
    } yield ()

  def add(key: K, value: V): F[Unit] =
    for {
      toAppend <- keyValueCodec.encode((key, value)) match {
                   case Successful(encoded) => Sync[F].delay(encoded.toByteArray)
                   case Failure(e)          => Sync[F].raiseError[Array[Byte]](EncodeError(e))
                 }
      _ <- outputStream.write(toAppend)
      _ <- outputStream.flush
      _ <- crc.update(toAppend).flatMap(_ => updateCrcFile)
      _ <- persistedDataState.modify(_.updated(key, value))
    } yield ()

  def addAll(newData: List[(K, V)]) =
    newData.traverse_(add)

  def data: F[Map[K, V]] =
    persistedDataState.get

  def close: F[Unit] =
    outputStream.close
}

object PersistentIndex {
  private final case class EncodeError(e: Err) extends Exception {
    override def getMessage: String = s"Could not encode: $e"
  }

  private final case class DecodeError(e: Err) extends Exception {
    override def getMessage: String = s"Could not decode: $e"
  }

  private def truncateDataLog[F[_]: Sync, K, V](
      randomAccessIO: RandomAccessIO[F],
      dataCollection: List[(K, V)],
      keyValueCodec: Codec[(K, V)]
  ): F[Unit] = {
    val lastEntry = dataCollection.last
    for {
      lastEntrySize <- keyValueCodec.encode(lastEntry) match {
                        case Successful(encoded) => ((encoded.size + 7) / 8).pure[F]
                        case Failure(e) =>
                          Sync[F].raiseError[Long](EncodeError(e))
                      }
      length <- randomAccessIO.length
      _      <- randomAccessIO.setLength(length - lastEntrySize)
    } yield ()
  }

  private def readCrc[F[_]: Sync: Log: RaiseIOError](crcPath: Path): F[Long] =
    for {
      _          <- createNewFile[F](crcPath)
      bytes      <- readAllBytesFromFile[F](crcPath)
      byteBuffer = ByteBuffer.wrap(bytes)
      result <- Sync[F].delay { byteBuffer.getLong() }.handleErrorWith {
                 case _: BufferUnderflowException =>
                   for {
                     _ <- Log[F].warn(s"CRC file $crcPath did not contain a valid CRC value")
                   } yield 0
                 case exception =>
                   Sync[F].raiseError(exception)
               }
    } yield result

  private def calculateCrc[F[_]: Sync, K, V](
      dataCollection: List[(K, V)],
      keyValueCodec: Codec[(K, V)]
  ): F[Crc32[F]] = {
    val crc = Crc32.empty[F]()
    dataCollection.traverse_[F, Unit] {
      case (key, value) =>
        keyValueCodec.encode((key, value)) match {
          case Successful(encoded) =>
            crc.update(encoded.toByteArray)
          case Failure(e) =>
            Sync[F].raiseError(EncodeError(e))
        }
    } >> crc.pure[F]
  }

  def readDataList[F[_]: Sync: RaiseIOError, K, V](
      dataPath: Path,
      keyValueCodec: Codec[(K, V)],
      corruptedError: => StorageError
  ): F[List[(K, V)]] =
    FileInputStreamIO.open[F](dataPath, StandardOpenOption.READ).use { inputStream =>
      for {
        bitVector <- Sync[F].delay { BitVector.fromInputStream(inputStream) }
        result <- list(keyValueCodec).decode(bitVector) match {
                   case Successful(DecodeResult(keyValuePairList, BitVector.empty)) =>
                     keyValuePairList.pure[F]
                   case _ =>
                     Sync[F].raiseError[List[(K, V)]](corruptedError)
                 }
      } yield result
    }

  private def validate[F[_]: Sync, K, V](
      randomAccessIO: RandomAccessIO[F],
      readCrc: Long,
      dataList: List[(K, V)],
      keyValueCodec: Codec[(K, V)],
      corruptedError: => StorageError
  ): F[(List[(K, V)], Crc32[F])] =
    for {
      fullCalculatedCrc <- calculateCrc(dataList, keyValueCodec)
      result <- Monad[F].ifM(fullCalculatedCrc.value.map(_ == readCrc))(
                 (dataList, fullCalculatedCrc).pure[F],
                 if (dataList.isEmpty) {
                   Sync[F].raiseError[(List[(K, V)], Crc32[F])](corruptedError)
                 } else {
                   // Trying to delete the last log entry which is most likely to be corrupted
                   val dataWithoutLast = dataList.init
                   calculateCrc(dataWithoutLast, keyValueCodec).flatMap { withoutLastCrc =>
                     Monad[F].ifM(withoutLastCrc.value.map(_ == readCrc))(
                       truncateDataLog(randomAccessIO, dataList, keyValueCodec) >>
                         (dataWithoutLast, withoutLastCrc).pure[F],
                       Sync[F].raiseError[(List[(K, V)], Crc32[F])](corruptedError)
                     )
                   }
                 }
               )
    } yield result

  def load[F[_]: Sync: Log: RaiseIOError, K: Codec, V: Codec](
      logPath: Path,
      crcPath: Path,
      corruptedError: => StorageError
  ): F[PersistentIndex[F, K, V]] = {
    val keyValueCodec = ("key" | Codec[K]) ~ ("value" | Codec[V])
    for {
      _        <- createNewFile[F](logPath)
      dataList <- readDataList(logPath, keyValueCodec, corruptedError)
      readCrc  <- readCrc(crcPath)
      result <- RandomAccessIO.openResource(logPath, RandomAccessIO.ReadWrite).use {
                 randomAccessIO =>
                   validate(randomAccessIO, readCrc, dataList, keyValueCodec, corruptedError)
               }
      (resultList, resultCrc) = result
      logOutputStream         <- FileOutputStreamIO.open[F](logPath, append = true)
    } yield new PersistentIndex(
      logPath,
      crcPath,
      new AtomicMonadState[F, Map[K, V]](AtomicAny(resultList.toMap)),
      resultCrc,
      keyValueCodec,
      logOutputStream
    )
  }
}
