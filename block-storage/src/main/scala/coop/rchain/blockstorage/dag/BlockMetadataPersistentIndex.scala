package coop.rchain.blockstorage.dag

import java.nio.file.Path

import cats.implicits._
import cats.Monad
import cats.effect.Sync
import cats.mtl.MonadState
import coop.rchain.blockstorage.DataLookupIsCorrupted
import coop.rchain.blockstorage.dag.codecs._
import coop.rchain.blockstorage.util.TopologicalSortUtil
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.shared.{AtomicMonadState, Log}
import monix.execution.atomic.AtomicAny

object BlockMetadataPersistentIndex {
  class PersistentBlockMetadataIndex[F[_]: Monad](
      private val persistentIndex: PersistentIndex[F, BlockHash, BlockMetadata],
      private val childMapState: MonadState[F, Map[BlockHash, Set[BlockHash]]],
      private val topoSortState: MonadState[F, Vector[Vector[BlockHash]]]
  ) {
    def add(block: BlockMetadata): F[Unit] =
      for {
        _ <- childMapState.modify { childMap =>
              block.parents
                .foldLeft(childMap) {
                  case (acc, p) =>
                    val currChildren = acc.getOrElse(p, Set.empty[BlockHash])
                    acc.updated(p, currChildren + block.blockHash)
                }
                .updated(block.blockHash, Set.empty[BlockHash])
            }
        _ <- topoSortState.modify(topoSort => TopologicalSortUtil.update(topoSort, 0L, block))
        _ <- persistentIndex.add(block.blockHash, block)
      } yield ()

    def childMapData: F[Map[BlockHash, Set[BlockHash]]] =
      childMapState.get

    def topoSortData: F[Vector[Vector[BlockHash]]] =
      topoSortState.get

    def blockMetadataData: F[Map[BlockHash, BlockMetadata]] =
      persistentIndex.data

    def close: F[Unit] =
      persistentIndex.close
  }

  private def extractChildMap(
      blockMetadataMap: Map[BlockHash, BlockMetadata]
  ): Map[BlockHash, Set[BlockHash]] =
    blockMetadataMap.foldLeft(blockMetadataMap.map(_._1 -> Set.empty[BlockHash])) {
      case (childMap, (_, blockMetadata)) =>
        blockMetadata.parents.foldLeft(childMap) {
          case (acc, p) =>
            val currentChildren = acc.getOrElse(p, Set.empty[BlockHash])
            acc.updated(p, currentChildren + blockMetadata.blockHash)
        }
    }

  private def extractTopoSort(
      blockMetadataMap: Map[BlockHash, BlockMetadata]
  ): Vector[Vector[BlockHash]] = {
    val blockMetadatas = blockMetadataMap.values.toVector
    val indexedTopoSort =
      blockMetadatas.groupBy(_.blockNum).mapValues(_.map(_.blockHash)).toVector.sortBy(_._1)
    assert(indexedTopoSort.zipWithIndex.forall { case ((readI, _), i) => readI == i })
    indexedTopoSort.map(_._2)
  }

  def load[F[_]: Sync: Log: RaiseIOError](
      logPath: Path,
      crcPath: Path
  ): F[PersistentBlockMetadataIndex[F]] =
    for {
      persistentIndex <- PersistentIndex.load[F, BlockHash, BlockMetadata](
                          logPath,
                          crcPath,
                          DataLookupIsCorrupted
                        )(Sync[F], Log[F], RaiseIOError[F], codecBlockHash, codecBlockMetadata)
      blockMetadataMap <- persistentIndex.data
      childMap         = extractChildMap(blockMetadataMap)
      topoSort         = extractTopoSort(blockMetadataMap)
    } yield new PersistentBlockMetadataIndex[F](
      persistentIndex,
      new AtomicMonadState(AtomicAny(childMap)),
      new AtomicMonadState(AtomicAny(topoSort))
    )

  def read[F[_]: Sync: RaiseIOError](logPath: Path): F[
    (Map[BlockHash, BlockMetadata], Map[BlockHash, Set[BlockHash]], Vector[Vector[BlockHash]])
  ] = {
    val keyValueCodec = codecBlockHash ~ codecBlockMetadata
    PersistentIndex
      .readDataList[F, BlockHash, BlockMetadata](
        logPath,
        keyValueCodec,
        DataLookupIsCorrupted
      )
      .map { blockMetadataList =>
        val blockMetadataMap = blockMetadataList.toMap
        (blockMetadataMap, extractChildMap(blockMetadataMap), extractTopoSort(blockMetadataMap))
      }
  }
}
