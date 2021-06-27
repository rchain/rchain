package coop.rchain.casper.blocks.merger

import cats.effect.Concurrent
import cats.syntax.all.{none, _}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.syntax._
import coop.rchain.dag.DagReader
import coop.rchain.models.BlockHash.BlockHash

final class CasperMergingDagReader[F[_]: Concurrent: BlockStore](
    hashDAGReader: DagReader[F, BlockHash]
) extends DagReader[F, MergingVertex] {
  override def children(vertex: MergingVertex): F[Option[Set[MergingVertex]]] =
    hashDAGReader.children(vertex.blockHash).flatMap {
      case None => none[Set[MergingVertex]].pure[F]
      case Some(children) =>
        BlockStore[F]
          .getUnsafe(children.toSeq)
          .compile
          .toList
          .map(
            _.map(
              b =>
                MergingVertex(
                  b.blockHash,
                  b.body.state.postStateHash,
                  b.body.state.preStateHash,
                  b.body.deploys.toSet
                )
            ).toSet.some
          )
    }

  override def parents(vertex: MergingVertex): F[Option[Set[MergingVertex]]] =
    hashDAGReader.parents(vertex.blockHash).flatMap {
      case None => none[Set[MergingVertex]].pure[F]
      case Some(children) =>
        BlockStore[F]
          .getUnsafe(children.toSeq)
          .compile
          .toList
          .map(
            _.map(
              b =>
                MergingVertex(
                  b.blockHash,
                  b.body.state.postStateHash,
                  b.body.state.preStateHash,
                  b.body.deploys.toSet
                )
            ).toSet.some
          )
    }
}
