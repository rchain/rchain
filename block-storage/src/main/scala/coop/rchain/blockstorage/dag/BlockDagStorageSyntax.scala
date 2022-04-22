package coop.rchain.blockstorage.dag

import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata

trait BlockDagStorageSyntax {
  implicit final def blockStorageSyntax[F[_]](
      bds: BlockDagStorage[F]
  ): BlockDagStorageOps[F] = new BlockDagStorageOps[F](bds)
}

final class BlockDagStorageOps[F[_]](
    // DagRepresentation extensions / syntax
    private val bds: BlockDagStorage[F]
) extends AnyVal {
  def lookupUnsafe(hash: BlockHash)(
      implicit sync: Sync[F]
  ): F[BlockMetadata] = {
    def errMsg = s"DAG storage is missing hash ${PrettyPrinter.buildString(hash)}"
    bds.lookup(hash) >>= (_.liftTo(BlockDagInconsistencyError(errMsg)))
  }

  def lookupUnsafe(
      hashes: Seq[BlockHash]
  )(implicit concurrent: Concurrent[F]): F[List[BlockMetadata]] = {
    val streams = hashes.map(h => fs2.Stream.eval(lookupUnsafe(h)))
    fs2.Stream.emits(streams).parJoinUnbounded.compile.toList
  }
}
