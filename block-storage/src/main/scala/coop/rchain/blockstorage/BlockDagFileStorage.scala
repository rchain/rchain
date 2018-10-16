package coop.rchain.blockstorage

import java.io._
import java.nio.{BufferUnderflowException, ByteBuffer}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.CRC32

import cats.{Id, Monad}
import cats.implicits._
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.{Ref, Semaphore}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockDagRepresentation.Validator
import coop.rchain.blockstorage.BlockStore.BlockHash
import coop.rchain.blockstorage.util.BlockMessageUtil.{bonds, parentHashes}
import coop.rchain.blockstorage.util.TopologicalSortUtil
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
    latestMessagesLogOsRef: Ref[F, OutputStream],
    latestMessagesLogSizeRef: Ref[F, Int],
    latestMessagesCrcRef: Ref[F, CRC32],
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

  private def updateLatestMessagesFile(validators: Set[Validator], blockHash: BlockHash): F[Unit] =
    for {
      latestMessagesLogOs <- latestMessagesLogOsRef.get
      latestMessagesCrc   <- latestMessagesCrcRef.get
      _ = for (validator <- validators) {
        val toAppend = validator.concat(blockHash).toByteArray
        latestMessagesCrc.update(toAppend)
        latestMessagesLogOs.write(toAppend)
        latestMessagesLogOs.flush()
        updateLatestMessagesCrcFile(latestMessagesCrc)
      }
      _ <- latestMessagesLogSizeRef.update(_ + 1)
    } yield ()

  private def crcBytes(crc: CRC32): Array[Byte] = {
    val byteBuffer = ByteBuffer.allocate(8)
    byteBuffer.putLong(crc.getValue)
    byteBuffer.array()
  }

  private def updateLatestMessagesCrcFile(newCrc: CRC32): Unit = {
    val tmpCrc = Files.createTempFile("rchain-block-dag-file-storage-", "-crc")
    Files.write(tmpCrc, crcBytes(newCrc))
    Files.move(tmpCrc, latestMessagesCrcFilePath, StandardCopyOption.REPLACE_EXISTING)
  }

  private def squashLatestMessagesDataFile(): F[Unit] =
    for {
      latestMessages      <- latestMessagesRef.get
      latestMessagesLogOs <- latestMessagesLogOsRef.get
      _                   = latestMessagesLogOs.close()
      tmpSquashedData     = Files.createTempFile("rchain-block-dag-file-storage-", "-squashed-data")
      tmpSquashedCrc      = Files.createTempFile("rchain-block-dag-file-storage-", "-squashed-crc")
      dataByteBuffer      = ByteBuffer.allocate(64 * latestMessages.size)
      _ = latestMessages.foreach {
        case (validator, blockHash) =>
          dataByteBuffer.put(validator.toByteArray)
          dataByteBuffer.put(blockHash.toByteArray)
      }
      _           = Files.write(tmpSquashedData, dataByteBuffer.array())
      squashedCrc = new CRC32()
      _           = squashedCrc.update(dataByteBuffer.array())
      _           = Files.write(tmpSquashedCrc, crcBytes(squashedCrc))
      _ = Files.move(
        tmpSquashedData,
        latestMessagesDataFilePath,
        StandardCopyOption.REPLACE_EXISTING
      )
      _ = Files.move(tmpSquashedCrc, latestMessagesCrcFilePath, StandardCopyOption.REPLACE_EXISTING)
      _ <- latestMessagesLogOsRef.set(new FileOutputStream(latestMessagesDataFilePath.toFile, true))
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
      _ <- updateLatestMessagesFile(newValidators + block.sender, block.blockHash)
      _ <- lock.release
    } yield ()

  def checkpoint(): F[Unit] =
    ().pure[F]

  def clear(): F[Unit] =
    for {
      _                   <- lock.acquire
      latestMessagesLogOs <- latestMessagesLogOsRef.get
      _                   = latestMessagesLogOs.close()
      _                   = Files.write(latestMessagesDataFilePath, Array.emptyByteArray)
      newCrc              = new CRC32()
      _                   = Files.write(latestMessagesCrcFilePath, crcBytes(newCrc))
      _                   <- dataLookupRef.set(Map.empty)
      _                   <- childMapRef.set(Map.empty)
      _                   <- topoSortRef.set(Vector.empty)
      _                   <- latestMessagesRef.set(Map.empty)
      _                   <- latestMessagesLogOsRef.set(new FileOutputStream(latestMessagesDataFilePath.toFile))
      _                   <- latestMessagesLogSizeRef.set(0)
      _                   <- latestMessagesCrcRef.set(newCrc)
      _                   <- lock.release
    } yield ()

  def close(): F[Unit] =
    for {
      _                   <- lock.acquire
      latestMessagesLogOs <- latestMessagesLogOsRef.get
      _                   = latestMessagesLogOs.close()
      _                   <- lock.release
    } yield ()
}

object BlockDagFileStorage {
  private implicit val logSource = LogSource(BlockDagFileStorage.getClass)

  final case class Config(
      latestMessagesDataPath: Path,
      latestMessagesCrcPath: Path,
      latestMessagesLogMaxSizeFactor: Int = 10
  )

  private def calculateLatestMessagesCrc(
      latestMessagesList: List[(Validator, BlockHash)]
  ): CRC32 = {
    val crc = new CRC32()
    latestMessagesList.foreach {
      case (validator, blockHash) =>
        crc.update(validator.concat(blockHash).toByteArray)
    }
    crc
  }

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

  private def validateData(
      latestMessagesRaf: RandomAccessFile,
      readCrc: Long,
      latestMessagesCrcPath: Path,
      latestMessagesList: List[(Validator, BlockHash)]
  ): (Map[Validator, BlockHash], CRC32) = {
    val fullCalculatedCrc = calculateLatestMessagesCrc(latestMessagesList)
    if (fullCalculatedCrc.getValue == readCrc) {
      (latestMessagesList.toMap, fullCalculatedCrc)
    } else {
      val withoutLastCalculatedCrc = calculateLatestMessagesCrc(latestMessagesList.init)
      if (withoutLastCalculatedCrc.getValue == readCrc) {
        latestMessagesRaf.setLength(latestMessagesRaf.length() - 64)
        (latestMessagesList.init.toMap, withoutLastCalculatedCrc)
      } else {
        // TODO: Restore latest messages from the persisted DAG
        latestMessagesRaf.setLength(0)
        (Map.empty[Validator, BlockHash], new CRC32())
      }
    }
  }

  def create[F[_]: Monad: Concurrent: Sync: Log](config: Config): F[BlockDagFileStorage[F]] =
    for {
      lock                          <- Semaphore[F](1)
      latestMessagesRaf             = new RandomAccessFile(config.latestMessagesDataPath.toFile, "rw")
      readCrc                       <- readLatestMessagesCrc[F](config.latestMessagesCrcPath)
      (latestMessagesList, logSize) = readLatestMessagesData(latestMessagesRaf)
      (latestMessagesMap, calculatedCrc) = validateData(latestMessagesRaf,
                                                        readCrc,
                                                        config.latestMessagesCrcPath,
                                                        latestMessagesList)
      _                        = latestMessagesRaf.close()
      latestMessagesDataOs     = new FileOutputStream(config.latestMessagesDataPath.toFile, true)
      latestMessagesRef        <- Ref.of[F, Map[Validator, BlockHash]](latestMessagesMap)
      latestMessagesLogSizeRef <- Ref.of[F, Int](logSize)
      latestMessagesCrcRef     <- Ref.of[F, CRC32](calculatedCrc)
      childMapRef              <- Ref.of[F, Map[BlockHash, Set[BlockHash]]](Map.empty)
      dataLookupRef            <- Ref.of[F, Map[BlockHash, BlockMetadata]](Map.empty)
      topoSortRef              <- Ref.of[F, Vector[Vector[BlockHash]]](Vector.empty)
      latestMessagesDataOsRef  <- Ref.of[F, OutputStream](latestMessagesDataOs)
    } yield
      new BlockDagFileStorage[F](
        lock,
        latestMessagesRef,
        childMapRef,
        dataLookupRef,
        topoSortRef,
        latestMessagesDataOsRef,
        latestMessagesLogSizeRef,
        latestMessagesCrcRef,
        config.latestMessagesDataPath,
        config.latestMessagesCrcPath,
        config.latestMessagesLogMaxSizeFactor
      )

  def unsafe[F[_]: Sync: Concurrent: Log](
      config: Config,
      lock: Semaphore[F],
      initialChildMap: Map[BlockHash, Set[BlockHash]],
      initialDataLookup: Map[BlockHash, BlockMetadata],
      initialTopoSort: Vector[Vector[BlockHash]]
  ): BlockDagFileStorage[F] = {
    val latestMessagesRaf             = new RandomAccessFile(config.latestMessagesDataPath.toFile, "rw")
    val (latestMessagesList, logSize) = readLatestMessagesData(latestMessagesRaf)
    val readCrc                       = readLatestMessagesCrcUnsafe(config.latestMessagesCrcPath)
    val (latestMessagesMap, calculatedCrc) =
      validateData(latestMessagesRaf, readCrc, config.latestMessagesCrcPath, latestMessagesList)
    latestMessagesRaf.close()
    val latestMessagesRef        = Ref.unsafe[F, Map[Validator, BlockHash]](latestMessagesMap)
    val latestMessagesLogSizeRef = Ref.unsafe[F, Int](logSize)
    val latestMessagesCrcRef     = Ref.unsafe[F, CRC32](calculatedCrc)
    val childMapRef              = Ref.unsafe[F, Map[BlockHash, Set[BlockHash]]](initialChildMap)
    val dataLookupRef            = Ref.unsafe[F, Map[BlockHash, BlockMetadata]](initialDataLookup)
    val topoSortRef              = Ref.unsafe[F, Vector[Vector[BlockHash]]](initialTopoSort)
    val latestMessagesDataOs     = new FileOutputStream(config.latestMessagesDataPath.toFile)
    val latestMessagesDataOsRef  = Ref.unsafe[F, OutputStream](latestMessagesDataOs)
    new BlockDagFileStorage[F](
      lock,
      latestMessagesRef,
      childMapRef,
      dataLookupRef,
      topoSortRef,
      latestMessagesDataOsRef,
      latestMessagesLogSizeRef,
      latestMessagesCrcRef,
      config.latestMessagesDataPath,
      config.latestMessagesCrcPath,
      config.latestMessagesLogMaxSizeFactor
    )
  }

  def createWithId(config: Config): BlockDagFileStorage[Id] = {
    import coop.rchain.catscontrib.effect.implicits._
    implicit val log = new Log.NOPLog[Id]()
    BlockDagFileStorage.create[Id](config)
  }
}
