package coop.rchain.blockstorage.dag

import java.nio.file.Path

import cats.Monad
import cats.effect.concurrent.Semaphore
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import cats.mtl.MonadState
import com.google.protobuf.ByteString
import coop.rchain.blockstorage._
import coop.rchain.blockstorage.dag.BlockDagFileStorage.{Checkpoint, CheckpointedDagInfo}
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.BlockMetadataPersistentIndex._
import coop.rchain.blockstorage.dag.DeployPersistentIndex._
import coop.rchain.blockstorage.dag.EquivocationTrackerPersistentIndex._
import coop.rchain.blockstorage.dag.InvalidBlocksPersistentIndex._
import coop.rchain.blockstorage.dag.LatestMessagesPersistentIndex._
import coop.rchain.blockstorage.util.BlockMessageUtil._
import coop.rchain.blockstorage.util.byteOps._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io.{IOError, _}
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16
import coop.rchain.lmdb.LMDBStore
import coop.rchain.metrics.Metrics.Source
import coop.rchain.metrics.{Metrics, MetricsSemaphore}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.{BlockHash, BlockMetadata, EquivocationRecord, Validator}
import coop.rchain.shared.ByteStringOps._
import coop.rchain.shared.{AtomicMonadState, Log, LogSource}
import monix.execution.atomic.AtomicAny
import org.lmdbjava.DbiFlags.MDB_CREATE
import org.lmdbjava.{Env, EnvFlags}

import scala.ref.WeakReference
import scala.util.matching.Regex

private final case class BlockDagFileStorageState[F[_]: Sync](
    sortOffset: Long,
    checkpoints: List[Checkpoint]
)

final class BlockDagFileStorage[F[_]: Concurrent: Sync: Log: RaiseIOError] private (
    lock: Semaphore[F],
    blockNumberIndex: LMDBStore[F],
    latestMessagesIndex: PersistentLatestMessagesIndex[F],
    blockMetadataIndex: PersistentBlockMetadataIndex[F],
    deployIndex: PersistentDeployIndex[F],
    invalidBlocksIndex: PersistentInvalidBlocksIndex[F],
    equivocationTrackerIndex: PersistentEquivocationTrackerIndex[F],
    state: MonadState[F, BlockDagFileStorageState[F]]
) extends BlockDagStorage[F] {
  implicit private val logSource: LogSource = LogSource(BlockDagFileStorage.getClass)
  private val iterableByteOrdering          = Ordering.Iterable[Byte]
  private val blockMetadataByNum: Ordering[BlockMetadata] =
    (l: BlockMetadata, r: BlockMetadata) => {
      def compareByteString(l: ByteString, r: ByteString): Int =
        iterableByteOrdering.compare(l.toByteArray, r.toByteArray)

      val ln = l.blockNum
      val rn = r.blockNum
      ln.compare(rn) match {
        case 0 => compareByteString(l.blockHash, r.blockHash)
        case v => v
      }
    }

  private[this] def getSortOffset: F[Long] =
    state.get.map(_.sortOffset)
  private[this] def getCheckpoints: F[List[Checkpoint]] =
    state.get.map(_.checkpoints)

  private[this] def modifyCheckpoints(f: List[Checkpoint] => List[Checkpoint]): F[Unit] =
    state.modify(s => s.copy(checkpoints = f(s.checkpoints)))

  private[this] def getBlockNumber(blockHash: BlockHash): F[Option[Long]] =
    for {
      blockNumberBytesOpt <- blockNumberIndex.get(blockHash.toDirectByteBuffer)
    } yield blockNumberBytesOpt.map(_.getLong)

  private[this] def putBlockNumber(blockHash: BlockHash, blockNumber: Long): F[Unit] =
    blockNumberIndex.put(
      blockHash.toDirectByteBuffer,
      blockNumber.toByteString.toDirectByteBuffer
    )

  private case class FileDagRepresentation(
      latestMessagesMap: Map[Validator, BlockHash],
      childMap: Map[BlockHash, Set[BlockHash]],
      dataLookup: Map[BlockHash, BlockMetadata],
      topoSortVector: Vector[Vector[BlockHash]],
      blockHashesByDeploy: Map[DeployId, BlockHash],
      invalidBlocksSet: Set[BlockMetadata],
      sortOffset: Long
  ) extends BlockDagRepresentation[F] {
    private def findAndAccessCheckpoint[R](
        blockHash: BlockHash,
        loadFromCheckpoint: CheckpointedDagInfo => Option[R]
    ): F[Option[R]] =
      for {
        blockNumberOpt <- getBlockNumber(blockHash)
        result <- blockNumberOpt match {
                   case Some(blockNumber) =>
                     if (blockNumber >= sortOffset) {
                       none[R].pure[F]
                     } else {
                       lock.withPermit(
                         loadCheckpoint(blockNumber).map(_.flatMap(loadFromCheckpoint))
                       )
                     }
                   case None =>
                     none[R].pure[F]
                 }
      } yield result

    def children(blockHash: BlockHash): F[Option[Set[BlockHash]]] =
      for {
        result <- childMap.get(blockHash) match {
                   case children: Some[Set[BlockHash]] =>
                     Monad[F].pure[Option[Set[BlockHash]]](children)
                   case None =>
                     findAndAccessCheckpoint(blockHash, _.childMap.get(blockHash))
                 }
      } yield result
    def lookup(blockHash: BlockHash): F[Option[BlockMetadata]] =
      dataLookup.get(blockHash) match {
        case blockMetadata: Some[BlockMetadata] =>
          Monad[F].pure[Option[BlockMetadata]](blockMetadata)
        case None =>
          findAndAccessCheckpoint(blockHash, _.dataLookup.get(blockHash))
      }
    def contains(blockHash: BlockHash): F[Boolean] =
      if (blockHash.size == BlockHash.Length) {
        dataLookup.get(blockHash) match {
          case Some(_) => true.pure[F]
          case None    => getBlockNumber(blockHash).map(_.isDefined)
        }
      } else {
        false.pure[F]
      }
    def lookupByDeployId(deployId: DeployId): F[Option[BlockHash]] =
      blockHashesByDeploy.get(deployId).pure[F]
    def topoSort(startBlockNumber: Long): F[Vector[Vector[BlockHash]]] =
      if (startBlockNumber >= sortOffset) {
        val offset = startBlockNumber - sortOffset
        assert(offset.isValidInt)
        topoSortVector.drop(offset.toInt).pure[F]
      } else if (sortOffset - startBlockNumber + topoSortVector.length < Int.MaxValue) { // Max Vector length
        lock.withPermit(
          for {
            checkpoints          <- getCheckpoints
            checkpointsWithIndex = checkpoints.zipWithIndex
            checkpointsToLoad    = checkpointsWithIndex.filter(startBlockNumber < _._1.end)
            checkpointsDagInfos <- checkpointsToLoad.traverse {
                                    case (startingCheckpoint, index) =>
                                      loadCheckpointDagInfo(startingCheckpoint, index)
                                  }
            topoSortPrefix = checkpointsDagInfos.toVector.flatMap { checkpointsDagInfo =>
              val offset = startBlockNumber - checkpointsDagInfo.sortOffset
              // offset is always a valid Int since the method result's length was validated before
              checkpointsDagInfo.topoSort.drop(offset.toInt) // negative drops are ignored
            }
            result = topoSortPrefix ++ topoSortVector
          } yield result
        )
      } else {
        Sync[F].raiseError(
          TopoSortLengthIsTooBig(sortOffset - startBlockNumber + topoSortVector.length)
        )
      }
    // TODO should startBlockNumber have topoSortVector.length - 1 (off by one error)?
    def topoSortTail(tailLength: Int): F[Vector[Vector[BlockHash]]] = {
      val startBlockNumber = Math.max(0L, sortOffset - (tailLength - topoSortVector.length))
      topoSort(startBlockNumber)
    }
    def deriveOrdering(startBlockNumber: Long): F[Ordering[BlockMetadata]] =
      blockMetadataByNum.pure[F]
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
    def invalidBlocks: F[Set[BlockMetadata]] =
      invalidBlocksSet.pure[F]
  }

  private object FileEquivocationsTracker extends EquivocationsTracker[F] {
    override def equivocationRecords: F[Set[EquivocationRecord]] =
      equivocationTrackerIndex.data
    override def insertEquivocationRecord(record: EquivocationRecord): F[Unit] =
      equivocationTrackerIndex.add(record)
    override def updateEquivocationRecord(
        record: EquivocationRecord,
        blockHash: BlockHash
    ): F[Unit] = {
      val updatedEquivocationDetectedBlockHashes =
        record.equivocationDetectedBlockHashes + blockHash
      val newRecord =
        record.copy(equivocationDetectedBlockHashes = updatedEquivocationDetectedBlockHashes)
      equivocationTrackerIndex.add(newRecord)
    }
  }

  private def loadDagInfo(checkpoint: Checkpoint): F[CheckpointedDagInfo] =
    for {
      readResult                             <- BlockMetadataPersistentIndex.read[F](checkpoint.path)
      (blockMetadataMap, childMap, topoSort) = readResult
    } yield CheckpointedDagInfo(childMap, blockMetadataMap, topoSort, checkpoint.start)

  private def loadCheckpointDagInfo(checkpoint: Checkpoint, index: Int): F[CheckpointedDagInfo] =
    checkpoint.dagInfo.flatMap(_.get) match {
      case Some(dagInfo) =>
        dagInfo.pure[F]
      case None =>
        for {
          loadedDagInfo <- loadDagInfo(checkpoint)
          newCheckpoint = checkpoint.copy(dagInfo = Some(WeakReference(loadedDagInfo)))
          _             <- modifyCheckpoints(_.patch(index, List(newCheckpoint), 1))
        } yield loadedDagInfo
    }

  private def loadCheckpoint(offset: Long): F[Option[CheckpointedDagInfo]] =
    for {
      checkpoints <- getCheckpoints
      neededCheckpoint = checkpoints.zipWithIndex.find {
        case (c, _) => c.start <= offset && offset < c.end
      }
      result <- neededCheckpoint match {
                 case None =>
                   Log[F].warn(
                     s"Requested a block with block number $offset, but there is no checkpoint for it"
                   ) >> None.pure[F]
                 case Some((checkpoint, i)) =>
                   loadCheckpointDagInfo(checkpoint, i).map(Option(_))
               }
    } yield result

  private def representation: F[BlockDagRepresentation[F]] =
    for {
      latestMessages      <- latestMessagesIndex.data
      childMap            <- blockMetadataIndex.childMapData
      blockMetadataMap    <- blockMetadataIndex.blockMetadataData
      topoSort            <- blockMetadataIndex.topoSortData
      blockHashesByDeploy <- deployIndex.data
      invalidBlocks       <- invalidBlocksIndex.data.map(_.keySet)
      sortOffset          <- getSortOffset
    } yield FileDagRepresentation(
      latestMessages,
      childMap,
      blockMetadataMap,
      topoSort,
      blockHashesByDeploy,
      invalidBlocks,
      sortOffset
    )

  def getRepresentation: F[BlockDagRepresentation[F]] =
    lock.withPermit(representation)

  def insert(
      block: BlockMessage,
      genesis: BlockMessage,
      invalid: Boolean
  ): F[BlockDagRepresentation[F]] =
    lock.withPermit(
      for {
        alreadyStored <- blockMetadataIndex.blockMetadataData.map(_.contains(block.blockHash))
        _ <- if (alreadyStored) {
              Log[F].warn(s"Block ${Base16.encode(block.blockHash.toByteArray)} is already stored")
            } else {
              val blockMetadata = BlockMetadata.fromBlock(block, invalid)
              assert(block.blockHash.size == BlockHash.Length)
              for {
                _ <- if (invalid) invalidBlocksIndex.add(blockMetadata, ()) else ().pure[F]
                //Block which contains newly bonded validators will not
                //have those validators in its justification
                newValidators = bonds(block)
                  .map(_.validator)
                  .toSet
                  .diff(block.justifications.map(_.validator).toSet)
                newValidatorsLatestMessages = newValidators.map(v => (v, genesis.blockHash))
                newValidatorsWithSenderLatestMessages <- if (block.sender.isEmpty) {
                                                          // Ignore empty sender for special cases such as genesis block
                                                          Log[F].warn(
                                                            s"Block ${Base16.encode(block.blockHash.toByteArray)} sender is empty"
                                                          ) >> newValidatorsLatestMessages.pure[F]
                                                        } else if (block.sender
                                                                     .size() == Validator.Length) {
                                                          (newValidatorsLatestMessages + (
                                                            (
                                                              block.sender,
                                                              block.blockHash
                                                            )
                                                          )).pure[F]
                                                        } else {
                                                          Sync[F].raiseError[Set[
                                                            (ByteString, ByteString)
                                                          ]](
                                                            BlockSenderIsMalformed(block)
                                                          )
                                                        }
                deployHashes = deployData(block).map(_.sig).toList
                _            <- putBlockNumber(block.blockHash, blockNumber(block))
                _            <- deployIndex.addAll(deployHashes.map(_ -> block.blockHash))
                _            <- latestMessagesIndex.addAll(newValidatorsWithSenderLatestMessages.toList)
                _            <- blockMetadataIndex.add(blockMetadata)
              } yield ()
            }
        dag <- representation
      } yield dag
    )

  override def accessEquivocationsTracker[A](f: EquivocationsTracker[F] => F[A]): F[A] =
    lock.withPermit(
      f(FileEquivocationsTracker)
    )

  def checkpoint(): F[Unit] =
    ().pure[F]

  def close(): F[Unit] =
    lock.withPermit(
      for {
        _ <- latestMessagesIndex.close
        _ <- blockMetadataIndex.close
        _ <- equivocationTrackerIndex.close
        _ <- deployIndex.close
        _ <- invalidBlocksIndex.close
        _ <- blockNumberIndex.close
      } yield ()
    )
}

object BlockDagFileStorage {
  val IntSize = 4L

  implicit private val BlockDagFileStorageMetricsSource: Source =
    Metrics.Source(BlockStorageMetricsSource, "dag-file")
  implicit private val logSource       = LogSource(BlockDagFileStorage.getClass)
  private val checkpointPattern: Regex = "([0-9]+)-([0-9]+)".r

  final case class Config(
      latestMessagesLogPath: Path,
      latestMessagesCrcPath: Path,
      blockMetadataLogPath: Path,
      blockMetadataCrcPath: Path,
      equivocationsTrackerLogPath: Path,
      equivocationsTrackerCrcPath: Path,
      invalidBlocksLogPath: Path,
      invalidBlocksCrcPath: Path,
      blockHashesByDeployLogPath: Path,
      blockHashesByDeployCrcPath: Path,
      checkpointsDirPath: Path,
      blockNumberIndexPath: Path,
      mapSize: Long,
      latestMessagesLogMaxSizeFactor: Int = 10,
      maxDbs: Int = 1,
      maxReaders: Int = 126,
      noTls: Boolean = true
  )

  private[blockstorage] final case class CheckpointedDagInfo(
      childMap: Map[BlockHash, Set[BlockHash]],
      dataLookup: Map[BlockHash, BlockMetadata],
      topoSort: Vector[Vector[BlockHash]],
      sortOffset: Long
  )

  private[blockstorage] final case class Checkpoint(
      start: Long,
      end: Long,
      path: Path,
      dagInfo: Option[WeakReference[CheckpointedDagInfo]]
  )

  private def loadCheckpoints[F[_]: Sync: Log: RaiseIOError](
      checkpointsDirPath: Path
  ): F[List[Checkpoint]] =
    for {
      _     <- makeDirectory[F](checkpointsDirPath)
      files <- listRegularFiles[F](checkpointsDirPath)
      checkpoints <- files.flatTraverse { filePath =>
                      filePath.getFileName.toString match {
                        case checkpointPattern(start, end) =>
                          List(Checkpoint(start.toLong, end.toLong, filePath, None)).pure[F]
                        case other =>
                          Log[F].warn(s"Ignoring file '$other': not a valid checkpoint name") >>
                            List.empty[Checkpoint].pure[F]
                      }
                    }
      sortedCheckpoints = checkpoints.sortBy(_.start)
      result <- if (sortedCheckpoints.headOption.forall(_.start == 0)) {
                 if (sortedCheckpoints.isEmpty ||
                     sortedCheckpoints.zip(sortedCheckpoints.tail).forall {
                       case (current, next) => current.end == next.start
                     }) {
                   sortedCheckpoints.pure[F]
                 } else {
                   Sync[F].raiseError(CheckpointsAreNotConsecutive(sortedCheckpoints.map(_.path)))
                 }
               } else {
                 Sync[F].raiseError(CheckpointsDoNotStartFromZero(sortedCheckpoints.map(_.path)))
               }
    } yield result

  private def loadBlockNumberIndexLmdbStore[F[_]: Sync: Log: RaiseIOError](
      config: Config
  ): F[LMDBStore[F]] =
    for {
      _ <- notExists[F](config.blockNumberIndexPath).ifM(
            makeDirectory[F](config.blockNumberIndexPath) >> ().pure[F],
            ().pure[F]
          )
      env <- Sync[F].delay {
              val flags = if (config.noTls) List(EnvFlags.MDB_NOTLS) else List.empty
              Env
                .create()
                .setMapSize(config.mapSize)
                .setMaxDbs(config.maxDbs)
                .setMaxReaders(config.maxReaders)
                .open(config.blockNumberIndexPath.toFile, flags: _*)
            }
      dbi <- Sync[F].delay {
              env.openDbi(s"block_dag_storage_block_number_index", MDB_CREATE)
            }
    } yield LMDBStore[F](env, dbi)

  def create[F[_]: Concurrent: Sync: Log: Metrics](
      config: Config
  ): F[BlockDagFileStorage[F]] = {
    implicit val raiseIOError: RaiseIOError[F] = IOError.raiseIOErrorThroughSync[F]
    for {
      lock             <- MetricsSemaphore.single[F]
      blockNumberIndex <- loadBlockNumberIndexLmdbStore(config)
      latestMessagesIndex <- LatestMessagesPersistentIndex.load[F](
                              config.latestMessagesLogPath,
                              config.latestMessagesCrcPath
                            )
      blockMetadataIndex <- BlockMetadataPersistentIndex.load[F](
                             config.blockMetadataLogPath,
                             config.blockMetadataCrcPath
                           )
      equivocationTrackerIndex <- EquivocationTrackerPersistentIndex.load[F](
                                   config.equivocationsTrackerLogPath,
                                   config.equivocationsTrackerCrcPath
                                 )
      invalidBlocksIndex <- InvalidBlocksPersistentIndex.load[F](
                             config.invalidBlocksLogPath,
                             config.invalidBlocksCrcPath
                           )
      deployIndex <- DeployPersistentIndex.load[F](
                      config.blockHashesByDeployLogPath,
                      config.blockHashesByDeployCrcPath
                    )
      sortedCheckpoints <- loadCheckpoints(config.checkpointsDirPath)
      state = BlockDagFileStorageState(
        sortOffset = sortedCheckpoints.lastOption.map(_.end).getOrElse(0L),
        checkpoints = sortedCheckpoints
      )
    } yield new BlockDagFileStorage[F](
      lock,
      blockNumberIndex,
      latestMessagesIndex,
      blockMetadataIndex,
      deployIndex,
      invalidBlocksIndex,
      equivocationTrackerIndex,
      new AtomicMonadState[F, BlockDagFileStorageState[F]](AtomicAny(state))
    )
  }

}
