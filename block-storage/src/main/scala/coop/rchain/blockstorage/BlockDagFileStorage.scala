package coop.rchain.blockstorage

import java.io._
import java.nio.{BufferUnderflowException, ByteBuffer}
import java.nio.file.{Files, Path, StandardCopyOption}

import cats.{Id, Monad}
import cats.implicits._
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.{Ref, Semaphore}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.util.BlockMessageUtil.{bonds, parentHashes}
import coop.rchain.blockstorage.util.{Crc32, TopologicalSortUtil}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.shared.{Log, LogSource}

import scala.collection.immutable.HashSet
import scala.util.{Failure, Success, Try}

final class BlockDagFileStorage[F[_]: Monad: Concurrent: Sync: Log] private (
    lock: Semaphore[F],
    latestMessagesRef: Ref[F, Map[Validator, BlockHash]],
    childMapRef: Ref[F, Map[BlockHash, Set[BlockHash]]],
    dataLookupRef: Ref[F, Map[BlockHash, BlockMetadata]],
    topoSortRef: Ref[F, Vector[Vector[BlockHash]]],
    latestMessagesLogOutputStreamRef: Ref[F, OutputStream],
    latestMessagesLogSizeRef: Ref[F, Int],
    latestMessagesCrcRef: Ref[F, Crc32[F]],
    latestMessagesDataFilePath: Path,
    latestMessagesCrcFilePath: Path,
    latestMessagesLogMaxSizeFactor: Int
) extends BlockDagStorage[F] {
  private final case class FileDagRepresentation(
      latestMessagesMap: Map[Validator, BlockHash],
      childMap: Map[BlockHash, Set[BlockHash]],
      dataLookup: Map[BlockHash, BlockMetadata],
      topoSortVector: Vector[Vector[BlockHash]]
  ) extends BlockDagRepresentation[F] {
    def children(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
      childMap.get(blockHash).pure[F]
    def lookup(blockHash: BlockHash): F[Option[BlockMetadata]] =
      dataLookup.get(blockHash).pure[F]
    def contains(blockHash: BlockHash): F[Boolean] =
      dataLookup.contains(blockHash).pure[F]
    def topoSort(startBlockNumber: Long): F[Vector[Vector[BlockHash]]] =
      topoSortVector.drop(startBlockNumber.toInt).pure[F]
    def topoSortTail(tailLength: Long): F[Vector[Vector[BlockHash]]] =
      topoSortVector.takeRight(tailLength.toInt).pure[F]
    def deriveOrdering(startBlockNumber: Long): F[Ordering[BlockMetadata]] =
      topoSort(startBlockNumber).map { topologicalSorting =>
        val order = topologicalSorting.flatten.zipWithIndex.toMap
        Ordering.by(b => order(b.blockHash))
      }
    def latestMessageHash(validator: Validator): F[Option[BlockHash]] =
      latestMessagesMap.get(validator).pure[F]
    def latestMessage(validator: Validator): F[Option[BlockMetadata]] =
      latestMessagesMap.get(validator).flatTraverse(lookup)
    def latestMessageHashes: F[Map[Validator, BlockHash]] =
      latestMessagesMap.pure[F]
    def latestMessages: F[Map[Validator, BlockMetadata]] =
      latestMessagesMap.toList
        .traverse {
          case (validator, hash) => lookup(hash).map(validator -> _.get)
        }
        .map(_.toMap)
  }

  private def updateLatestMessagesFile(validators: List[Validator], blockHash: BlockHash): F[Unit] =
    for {
      latestMessagesLogOutputStream <- latestMessagesLogOutputStreamRef.get
      latestMessagesCrc             <- latestMessagesCrcRef.get
      _ <- validators.traverse_ { validator =>
            val toAppend = validator.concat(blockHash).toByteArray
            latestMessagesLogOutputStream.write(toAppend)
            latestMessagesLogOutputStream.flush()
            latestMessagesCrc.update(toAppend).map { _ =>
              updateLatestMessagesCrcFile(latestMessagesCrc)
            }
          }
      _ <- latestMessagesLogSizeRef.update(_ + 1)
    } yield ()

  private def updateLatestMessagesCrcFile(newCrc: Crc32[F]): F[Unit] =
    newCrc.bytes.map { newCrcBytes =>
      val tmpCrc = Files.createTempFile("rchain-block-dag-file-storage-", "-crc")
      Files.write(tmpCrc, newCrcBytes)
      Files.move(tmpCrc, latestMessagesCrcFilePath, StandardCopyOption.REPLACE_EXISTING)
    }

  private def squashLatestMessagesDataFile(): F[Unit] =
    for {
      latestMessages                <- latestMessagesRef.get
      latestMessagesLogOutputStream <- latestMessagesLogOutputStreamRef.get
      _                             = latestMessagesLogOutputStream.close()
      tmpSquashedData               = Files.createTempFile("rchain-block-dag-file-storage-", "-squashed-data")
      tmpSquashedCrc                = Files.createTempFile("rchain-block-dag-file-storage-", "-squashed-crc")
      dataByteBuffer                = ByteBuffer.allocate(64 * latestMessages.size)
      _ = latestMessages.foreach {
        case (validator, blockHash) =>
          dataByteBuffer.put(validator.toByteArray)
          dataByteBuffer.put(blockHash.toByteArray)
      }
      _                = Files.write(tmpSquashedData, dataByteBuffer.array())
      squashedCrc      = Crc32.empty[F]()
      _                <- squashedCrc.update(dataByteBuffer.array())
      squashedCrcBytes <- squashedCrc.bytes
      _                = Files.write(tmpSquashedCrc, squashedCrcBytes)
      _ = Files.move(
        tmpSquashedData,
        latestMessagesDataFilePath,
        StandardCopyOption.REPLACE_EXISTING
      )
      _ = Files.move(tmpSquashedCrc, latestMessagesCrcFilePath, StandardCopyOption.REPLACE_EXISTING)
      _ <- latestMessagesLogOutputStreamRef.set(
            new FileOutputStream(latestMessagesDataFilePath.toFile, true)
          )
      _ <- latestMessagesCrcRef.set(squashedCrc)
      _ <- latestMessagesLogSizeRef.set(0)
    } yield ()

  def getRepresentation: F[BlockDagRepresentation[F]] =
    for {
      _              <- lock.acquire
      latestMessages <- latestMessagesRef.get
      childMap       <- childMapRef.get
      dataLookup     <- dataLookupRef.get
      topoSort       <- topoSortRef.get
      _              <- lock.release
    } yield FileDagRepresentation(latestMessages, childMap, dataLookup, topoSort)

  def insert(block: BlockMessage): F[Unit] =
    for {
      _                     <- lock.acquire
      latestMessages        <- latestMessagesRef.get
      latestMessagesLogSize <- latestMessagesLogSizeRef.get
      _ <- if (latestMessagesLogSize > latestMessages.size * latestMessagesLogMaxSizeFactor) {
            squashLatestMessagesDataFile()
          } else {
            ().pure[F]
          }
      _ <- dataLookupRef.update(_.updated(block.blockHash, BlockMetadata.fromBlock(block)))
      _ <- childMapRef.update(
            childMap =>
              parentHashes(block).foldLeft(childMap) {
                case (acc, p) =>
                  val currChildren = acc.getOrElse(p, HashSet.empty[BlockHash])
                  acc.updated(p, currChildren + block.blockHash)
              }
          )
      _ <- topoSortRef.update(topoSort => TopologicalSortUtil.update(topoSort, 0L, block))
      //Block which contains newly bonded validators will not
      //have those validators in its justification
      newValidators = bonds(block)
        .map(_.validator)
        .toSet
        .diff(block.justifications.map(_.validator).toSet)
      _ <- latestMessagesRef.update { latestMessages =>
            newValidators.foldLeft(
              //update creator of the block
              latestMessages.updated(block.sender, block.blockHash)
            ) {
              //Update new validators with block in which
              //they were bonded (i.e. this block)
              case (acc, v) => acc.updated(v, block.blockHash)
            }
          }
      _ <- updateLatestMessagesFile((newValidators + block.sender).toList, block.blockHash)
      _ <- lock.release
    } yield ()

  def checkpoint(): F[Unit] =
    ().pure[F]

  def clear(): F[Unit] =
    for {
      _                             <- lock.acquire
      latestMessagesLogOutputStream <- latestMessagesLogOutputStreamRef.get
      _                             = latestMessagesLogOutputStream.close()
      _                             = Files.write(latestMessagesDataFilePath, Array.emptyByteArray)
      newCrc                        = Crc32.empty[F]()
      newCrcBytes                   <- newCrc.bytes
      _                             = Files.write(latestMessagesCrcFilePath, newCrcBytes)
      _                             <- dataLookupRef.set(Map.empty)
      _                             <- childMapRef.set(Map.empty)
      _                             <- topoSortRef.set(Vector.empty)
      _                             <- latestMessagesRef.set(Map.empty)
      _ <- latestMessagesLogOutputStreamRef.set(
            new FileOutputStream(latestMessagesDataFilePath.toFile)
          )
      _ <- latestMessagesLogSizeRef.set(0)
      _ <- latestMessagesCrcRef.set(newCrc)
      _ <- lock.release
    } yield ()

  def close(): F[Unit] =
    for {
      _                             <- lock.acquire
      latestMessagesLogOutputStream <- latestMessagesLogOutputStreamRef.get
      _                             = latestMessagesLogOutputStream.close()
      _                             <- lock.release
    } yield ()
}

object BlockDagFileStorage {
  private implicit val logSource = LogSource(BlockDagFileStorage.getClass)

  final case class Config(
      latestMessagesDataPath: Path,
      latestMessagesCrcPath: Path,
      latestMessagesLogMaxSizeFactor: Int = 10
  )

  private def calculateLatestMessagesCrc[F[_]: Monad](
      latestMessagesList: List[(Validator, BlockHash)]
  ): Crc32[F] =
    Crc32[F](
      latestMessagesList
        .foldLeft(ByteString.EMPTY) {
          case (byteString, (validator, blockHash)) =>
            byteString.concat(validator.concat(blockHash))
        }
        .toByteArray
    )

  private def readLatestMessagesData(
      latestMessagesDataDi: DataInput
  ): (List[(Validator, BlockHash)], Int) = {
    def readRec(
        result: List[(Validator, BlockHash)],
        logSize: Int
    ): (List[(Validator, BlockHash)], Int) =
      Try {
        val validatorPk = Array.fill[Byte](32)(0)
        val blockHash   = Array.fill[Byte](32)(0)
        latestMessagesDataDi.readFully(validatorPk)
        latestMessagesDataDi.readFully(blockHash)
        (validatorPk, blockHash)
      } match {
        case Success((validator, blockHash)) =>
          val pair = (ByteString.copyFrom(validator), ByteString.copyFrom(blockHash))
          readRec(
            pair :: result,
            logSize + 1
          )
        case Failure(_: EOFException) =>
          (result.reverse, logSize)
      }
    readRec(List.empty, 0)
  }

  private def readLatestMessagesCrc[F[_]: Monad: Sync: Log](crcPath: Path): F[Long] = {
    crcPath.toFile.createNewFile()
    val byteBuffer = ByteBuffer.wrap(Files.readAllBytes(crcPath))
    Try(byteBuffer.getLong()) match {
      case Success(value) => value.pure[F]
      case Failure(_: BufferUnderflowException) =>
        for {
          _ <- Log[F].warn(s"CRC file $crcPath did not contain a valid CRC value")
        } yield 0
      case Failure(exception) =>
        exception.raiseError[F, Long]
    }
  }

  private def readLatestMessagesCrcUnsafe(crcPath: Path): Long = {
    crcPath.toFile.createNewFile()
    val byteBuffer = ByteBuffer.wrap(Files.readAllBytes(crcPath))
    byteBuffer.getLong()
  }

  private def validateData[F[_]: Monad](
      latestMessagesRaf: RandomAccessFile,
      readCrc: Long,
      latestMessagesCrcPath: Path,
      latestMessagesList: List[(Validator, BlockHash)]
  ): F[(Map[Validator, BlockHash], Crc32[F])] = {
    val fullCalculatedCrc = calculateLatestMessagesCrc[F](latestMessagesList)
    fullCalculatedCrc.value.flatMap { fullCalculatedCrcValue =>
      if (fullCalculatedCrcValue == readCrc) {
        (latestMessagesList.toMap, fullCalculatedCrc).pure[F]
      } else {
        val withoutLastCalculatedCrc = calculateLatestMessagesCrc[F](latestMessagesList.init)
        withoutLastCalculatedCrc.value.map { withoutLastCalculatedCrcValue =>
          if (withoutLastCalculatedCrcValue == readCrc) {
            latestMessagesRaf.setLength(latestMessagesRaf.length() - 64)
            (latestMessagesList.init.toMap, withoutLastCalculatedCrc)
          } else {
            // TODO: Restore latest messages from the persisted DAG
            latestMessagesRaf.setLength(0)
            (Map.empty[Validator, BlockHash], Crc32.empty[F]())
          }
        }
      }
    }
  }

  def create[F[_]: Monad: Concurrent: Sync: Log](config: Config): F[BlockDagFileStorage[F]] =
    for {
      lock                          <- Semaphore[F](1)
      latestMessagesRaf             = new RandomAccessFile(config.latestMessagesDataPath.toFile, "rw")
      readCrc                       <- readLatestMessagesCrc[F](config.latestMessagesCrcPath)
      (latestMessagesList, logSize) = readLatestMessagesData(latestMessagesRaf)
      validateDataResult <- validateData[F](
                             latestMessagesRaf,
                             readCrc,
                             config.latestMessagesCrcPath,
                             latestMessagesList
                           )
      (latestMessagesMap, calculatedCrc) = validateDataResult
      _                                  = latestMessagesRaf.close()
      latestMessagesDataOutputStream = new FileOutputStream(
        config.latestMessagesDataPath.toFile,
        true
      )
      latestMessagesRef                 <- Ref.of[F, Map[Validator, BlockHash]](latestMessagesMap)
      latestMessagesLogSizeRef          <- Ref.of[F, Int](logSize)
      latestMessagesCrcRef              <- Ref.of[F, Crc32[F]](calculatedCrc)
      childMapRef                       <- Ref.of[F, Map[BlockHash, Set[BlockHash]]](Map.empty)
      dataLookupRef                     <- Ref.of[F, Map[BlockHash, BlockMetadata]](Map.empty)
      topoSortRef                       <- Ref.of[F, Vector[Vector[BlockHash]]](Vector.empty)
      latestMessagesDataOutputStreamRef <- Ref.of[F, OutputStream](latestMessagesDataOutputStream)
    } yield
      new BlockDagFileStorage[F](
        lock,
        latestMessagesRef,
        childMapRef,
        dataLookupRef,
        topoSortRef,
        latestMessagesDataOutputStreamRef,
        latestMessagesLogSizeRef,
        latestMessagesCrcRef,
        config.latestMessagesDataPath,
        config.latestMessagesCrcPath,
        config.latestMessagesLogMaxSizeFactor
      )

  def createWithId(config: Config): BlockDagFileStorage[Id] = {
    import coop.rchain.catscontrib.effect.implicits._
    implicit val log = new Log.NOPLog[Id]()
    BlockDagFileStorage.create[Id](config)
  }
}
