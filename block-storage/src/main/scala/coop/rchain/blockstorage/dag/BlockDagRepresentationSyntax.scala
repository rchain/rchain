package coop.rchain.blockstorage.dag

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.TopoSortFragmentParameterError
import coop.rchain.blockstorage.syntax._
import coop.rchain.models.BlockHash.BlockHash
import fs2.Stream

trait DagRepresentationSyntax {
  implicit final def blockStorageSyntaxDagRepresentation[F[_]](
      dag: DagRepresentation
  ): DagRepresentationOps[F] = new DagRepresentationOps[F](dag)
}

final class DagRepresentationOps[F[_]](
    // DagRepresentation extensions / syntax
    private val dag: DagRepresentation
) extends AnyVal {

  // TODO: refactor callers to work with multiple finalized blocks (fringe)
  def lastFinalizedBlockUnsafe(implicit s: Sync[F]): F[BlockHash] =
    dag.lastFinalizedBlockHash.liftTo(new Exception("Finalized fringe is not available."))

  def nonFinalizedBlocks(implicit sync: Sync[F], bds: BlockDagStorage[F]): F[Set[BlockHash]] =
    Stream
      .unfoldLoopEval(dag.dagMessageState.latestMsgs.map(_.id).toList) { lvl =>
        for {
          out  <- lvl.filterNot(dag.isFinalized).pure
          next <- out.traverse(bds.lookup(_).map(_.map(_.justifications))).map(_.flatten.flatten)
        } yield (out, next.nonEmpty.guard[Option].as(next))
      }
      .flatMap(Stream.emits)
      .compile
      .to(Set)

  def descendants(blockHash: BlockHash): Set[BlockHash] =
    Stream
      .unfoldLoop(List(blockHash)) { lvl =>
        val out  = lvl.flatMap(dag.children).flatten
        val next = out
        (out, next.nonEmpty.guard[Option].as(next))
      }
      .flatMap(Stream.emits)
      .compile
      .to(Set)

  def ancestors(blockHashes: List[BlockHash], filterF: BlockHash => F[Boolean])(
      implicit sync: Sync[F],
      bds: BlockDagStorage[F]
  ): F[Set[BlockHash]] =
    Stream
      .unfoldEval(blockHashes) { lvl =>
        val parents = lvl
          .traverse(bds.lookupUnsafe)
          .flatMap(_.flatMap(_.justifications).distinct.filterA(filterF))
        parents.map(p => p.nonEmpty.guard[Option].as(p -> p))
      }
      .flatMap(Stream.emits)
      .compile
      .to(Set)

  def topoSortUnsafe(
      startBlockNumber: Long,
      maybeEndBlockNumber: Option[Long]
  )(implicit sync: Sync[F]): F[Vector[Vector[BlockHash]]] =
    dag
      .topoSort(startBlockNumber, maybeEndBlockNumber)
      .liftTo(TopoSortFragmentParameterError(startBlockNumber, Long.MaxValue))
}
