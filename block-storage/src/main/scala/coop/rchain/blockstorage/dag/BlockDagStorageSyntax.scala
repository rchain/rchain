package coop.rchain.blockstorage.dag

import cats.effect.{Async, Sync}
import cats.syntax.all._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata

trait BlockDagStorageSyntax {
  implicit final def blockStorageSyntaxBlockDagStorage[F[_]](
      bds: BlockDagStorage[F]
  ): BlockDagStorageOps[F] = new BlockDagStorageOps[F](bds)
}

final case class BlockDagInconsistencyError(message: String) extends Exception(message)

final class BlockDagStorageOps[F[_]](
    // DagRepresentation extensions / syntax
    private val bds: BlockDagStorage[F]
) extends AnyVal {
  def lookupUnsafe(hash: BlockHash)(implicit sync: Sync[F]): F[BlockMetadata] = {
    def errMsg = s"DAG storage is missing hash ${PrettyPrinter.buildString(hash)}"
    bds.lookup(hash) >>= (_.liftTo(BlockDagInconsistencyError(errMsg)))
  }

  def lookupUnsafe(
      hashes: Seq[BlockHash]
  )(implicit concurrent: Async[F]): F[List[BlockMetadata]] = {
    val streams = hashes.map(h => fs2.Stream.eval(lookupUnsafe(h)))
    fs2.Stream.emits(streams).parJoinUnbounded.compile.toList
  }

  /**
    * Inserts genesis block to [[BlockDagStorage]] with filled [[BlockMetadata]].
    *
    * Fringe is empty and fringe state is genesis pre-state.
    */
  def insertGenesis(genesisBlock: BlockMessage)(implicit sync: Sync[F]): F[Unit] =
    for {
      bmd <- Sync[F].delay {
              BlockMetadata
                .fromBlock(genesisBlock)
                .copy(
                  validated = true,
                  validationFailed = false,
                  fringe = Set.empty,
                  fringeStateHash = genesisBlock.preStateHash
                )
            }
      _ <- bds.insert(bmd, genesisBlock)
    } yield ()
}
