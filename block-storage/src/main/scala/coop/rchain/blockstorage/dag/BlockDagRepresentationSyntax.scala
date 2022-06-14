package coop.rchain.blockstorage.dag

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.TopoSortFragmentParameterError
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import fs2.Stream

trait DagRepresentationSyntax {
  implicit final def blockStorageSyntaxDagRepresentation[F[_]](
      dag: DagRepresentation
  ): DagRepresentationOps[F] = new DagRepresentationOps[F](dag)
}

final case class NoLatestMessage(message: String) extends Exception(message)

final class DagRepresentationOps[F[_]](
    // DagRepresentation extensions / syntax
    private val dag: DagRepresentation
) extends AnyVal {

  def lastFinalizedBlockUnsafe(implicit s: Sync[F]): F[BlockHash] =
    DagRepresentation.lastFinalizedBlockUnsafe(dag)

  def latestMessageHash(
      validator: Validator
  )(implicit s: Sync[F], bds: BlockDagStorage[F]): F[Option[BlockHash]] =
    DagRepresentation.latestMessageHash(dag, validator)

  def latestMessageHashes(
      implicit s: Sync[F],
      bds: BlockDagStorage[F]
  ): F[Map[Validator, BlockHash]] =
    DagRepresentation.latestMessageHashes(dag)

  def latestMessageHashUnsafe(
      v: Validator
  )(implicit sync: Sync[F], bds: BlockDagStorage[F]): F[BlockHash] = {
    def errMsg = s"No latest message for validator ${PrettyPrinter.buildString(v)}"
    latestMessageHash(v) >>= (_.liftTo(NoLatestMessage(errMsg)))
  }

  def latestMessage(
      validator: Validator
  )(implicit sync: Sync[F], bds: BlockDagStorage[F]): F[Option[BlockMetadata]] =
    latestMessageHash(validator) >>= (_.traverse(bds.lookupUnsafe))

  def latestMessages(
      implicit sync: Sync[F],
      bds: BlockDagStorage[F]
  ): F[Map[Validator, BlockMetadata]] = {
    import cats.instances.vector._
    latestMessageHashes >>= (
      _.toVector
        .traverse {
          case (validator, hash) => bds.lookupUnsafe(hash).map(validator -> _)
        }
        .map(_.toMap)
      )
  }

  def nonFinalizedBlocks(implicit sync: Sync[F], bds: BlockDagStorage[F]): F[Set[BlockHash]] =
    Stream
      .unfoldLoopEval(dag.latestMessagesHashes.valuesIterator.toList) { lvl =>
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

  def ancestors(blockHash: BlockHash, filterF: BlockHash => F[Boolean])(
      implicit sync: Sync[F],
      bds: BlockDagStorage[F]
  ): F[Set[BlockHash]] =
    Stream
      .unfoldEval(List(blockHash)) { lvl =>
        val parents = lvl
          .traverse(bds.lookupUnsafe)
          .flatMap(_.flatMap(_.justifications).distinct.filterA(filterF))
        parents.map(p => p.nonEmpty.guard[Option].as(p, p))
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

  // TODO replace all calls with direct calls for BlockDagStorage
  def lookup(blockHash: BlockHash)(implicit bds: BlockDagStorage[F]) = bds.lookup(blockHash)
  def lookupUnsafe(blockHash: BlockHash)(implicit s: Sync[F], bds: BlockDagStorage[F]) =
    bds.lookupUnsafe(blockHash)
}
