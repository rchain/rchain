package coop.rchain.blockstorage.finality

import cats.Show
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.BlockMetadataStore
import coop.rchain.blockstorage.dag.codecs.{codecBlockHash, codecBlockMetadata, codecSeqNum}
import coop.rchain.casper.PrettyPrinter
import coop.rchain.dag.DagOps
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.shared.Log
import coop.rchain.shared.syntax._
import coop.rchain.store.{KeyValueStore, KeyValueStoreManager, KeyValueTypedStore}

class LastFinalizedKeyValueStorage[F[_]: Sync] private (
    lastFinalizedBlockDb: KeyValueTypedStore[F, Int, BlockHash]
) extends LastFinalizedStorage[F] {
  val fixedKey = 1

  override def put(blockHash: BlockHash): F[Unit] =
    lastFinalizedBlockDb.put(Seq((fixedKey, blockHash)))

  override def get(): F[Option[BlockHash]] =
    lastFinalizedBlockDb.get(fixedKey)

  val DONE = ByteString.copyFrom(Array.fill[Byte](32)(-1))

  def requireMigration: F[Boolean] = get().map(_.exists(_ != DONE))

  def migrateLfb(kvm: KeyValueStoreManager[F], blockStore: BlockStore[F])(
      implicit log: Log[F]
  ): F[Unit] = {
    val errNoLfbInStorage =
      "No LFB in LastFinalizedStorage nor ApprovedBlock found when attempting migration."
    val errNoMetadataForLfb = "No metadata found for LFB when attempting migration."

    for {
      blockMetadataDb <- kvm.database[BlockHash, BlockMetadata](
                          "block-metadata",
                          codecBlockHash,
                          codecBlockMetadata
                        )
      // record LFB
      persistedLfbOpt      <- get()
      approvedBlockHashOpt <- blockStore.getApprovedBlock.map(_.map(_.candidate.block.blockHash))
      // record hash stored in LastFinalizedStorage, or ApprovedBlock or throw error
      lfb  <- persistedLfbOpt.orElse(approvedBlockHashOpt).liftTo(new Exception(errNoLfbInStorage))
      curV <- blockMetadataDb.get(lfb).flatMap(_.liftTo[F](new Exception(errNoMetadataForLfb)))
      _    <- blockMetadataDb.put(lfb, curV.copy(directlyFinalized = true, finalized = true))
      blocksInfoMap <- blockMetadataDb
                        .collect {
                          case (hash, metaData) =>
                            (hash, BlockMetadataStore.blockMetadataToInfo(metaData()))
                        }
                        .map(_.toMap)
      _ <- Log[F].info("Migration of LFB done.")

      // record finalized blocks
      finalizedBlockSet <- DagOps
                            .bfTraverseF(List(lfb)) { bh =>
                              blocksInfoMap
                                .get(bh)
                                // with trimmed state edge parents might not be in the blockmetadataDB, so filter them out
                                .map(_.parents.toList.filterA(blockMetadataDb.contains(_)))
                                .getOrElse(List.empty[BlockHash].pure)
                            }
                            .toList

      processChunk = (chunk: List[BlockHash]) => {
        implicit val s = new Show[BlockHash] {
          override def show(t: BlockHash): String = PrettyPrinter.buildString(t)
        }

        for {
          curVs <- blockMetadataDb.getUnsafeBatch(chunk)
          // WARNING: migration should be done before block merge, as it assumes all blocks are directly finalized.
          newVs = curVs.map(_.copy(directlyFinalized = true, finalized = true))
          _     <- blockMetadataDb.put(newVs.map(v => (v.blockHash, v)))
        } yield ()
      }

      chunkSize = 10000L
      _ <- fs2.Stream
            .fromIterator(finalizedBlockSet.grouped(chunkSize.toInt))
            .evalMapAccumulate(0L)(
              (processed, chunk) => {
                val processSoFar = processed + chunk.size.toLong
                processChunk(chunk) >> Log[F]
                  .info(s"Finalized blocks recorded: ${processSoFar} of ${finalizedBlockSet.size}.")
                  .as((processSoFar, ()))
              }
            )
            .compile
            .drain

      // Mark migration as done
      _ <- put(DONE)
    } yield ()
  }
}

object LastFinalizedKeyValueStorage {
  def apply[F[_]: Sync](
      lastFinalizedBlockDb: KeyValueTypedStore[F, Int, BlockHash]
  ): LastFinalizedKeyValueStorage[F] = new LastFinalizedKeyValueStorage(lastFinalizedBlockDb)

  def apply[F[_]: Sync](keyValueStore: KeyValueStore[F]): LastFinalizedKeyValueStorage[F] =
    apply(keyValueStore.toTypedStore(codecSeqNum, codecBlockHash))
}
