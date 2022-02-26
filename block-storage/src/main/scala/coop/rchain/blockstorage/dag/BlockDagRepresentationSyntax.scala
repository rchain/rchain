package coop.rchain.blockstorage.dag

import cats.data.OptionT
import cats.effect.{Concurrent, Sync}
import cats.syntax.all._
import coop.rchain.casper.PrettyPrinter
import coop.rchain.casper.protocol.Justification
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.BlockMetadata
import coop.rchain.models.Validator.Validator
import coop.rchain.models.syntax._
import coop.rchain.shared.syntax._
import fs2.Stream

trait BlockDagRepresentationSyntax {
  implicit final def blockStorageSyntaxBlockDagRepresentation[F[_]](
      dag: BlockDagRepresentation[F]
  ): BlockDagRepresentationOps[F] = new BlockDagRepresentationOps[F](dag)
}

final case class BlockDagInconsistencyError(message: String) extends Exception(message)
final case class NoLatestMessage(message: String)            extends Exception(message)

final class BlockDagRepresentationOps[F[_]](
    // BlockDagRepresentation extensions / syntax
    private val dag: BlockDagRepresentation[F]
) extends AnyVal {

  /**
    * Get block metadata, "unsafe" because method expects block already in the DAG.
    *
    * Unfortunately there is no way to get stack trace when error is thrown in async execution.
    * Monix does not have support and cats.effect support is in creation.
    * https://github.com/typelevel/cats-effect/pull/854
    * So extra source parameters are a desperate measure to indicate who is the caller.
    */
  def lookupUnsafe(hash: BlockHash)(
      implicit sync: Sync[F]
  ): F[BlockMetadata] = {
    def errMsg = s"DAG storage is missing hash ${PrettyPrinter.buildString(hash)}"
    dag.lookup(hash) >>= (_.liftTo(BlockDagInconsistencyError(errMsg)))
  }

  def lookupUnsafe(
      hashes: Seq[BlockHash]
  )(implicit concurrent: Concurrent[F]): F[List[BlockMetadata]] = {
    val streams = hashes.map(h => fs2.Stream.eval(lookupUnsafe(h)))
    fs2.Stream.emits(streams).parJoinUnbounded.compile.toList
  }

  def latestMessageHashUnsafe(v: Validator)(implicit sync: Sync[F]): F[BlockHash] = {
    def errMsg = s"No latest message for validator ${PrettyPrinter.buildString(v)}"
    dag.latestMessageHash(v) >>= (_.liftTo(NoLatestMessage(errMsg)))
  }

  def latestMessage(validator: Validator)(implicit sync: Sync[F]): F[Option[BlockMetadata]] =
    dag.latestMessageHash(validator) >>= (_.traverse(lookupUnsafe))

  def latestMessages(implicit sync: Sync[F]): F[Map[Validator, BlockMetadata]] = {
    import cats.instances.vector._
    dag.latestMessageHashes >>= (
      _.toVector
        .traverse { case (validator, hash) => lookupUnsafe(hash).map(validator -> _) }
        .map(_.toMap)
      )
  }

  def invalidLatestMessages(implicit sync: Sync[F]): F[Map[Validator, BlockHash]] =
    latestMessages.flatMap(
      lm =>
        invalidLatestMessages(lm.map {
          case (validator, block) => (validator, block.blockHash)
        })
    )

  def invalidLatestMessages(latestMessagesHashes: Map[Validator, BlockHash])(
      implicit sync: Sync[F]
  ): F[Map[Validator, BlockHash]] =
    dag.invalidBlocks.map { invalidBlocks =>
      latestMessagesHashes.filter {
        case (_, blockHash) => invalidBlocks.map(_.blockHash).contains(blockHash)
      }
    }

  def invalidBlocksMap(implicit sync: Sync[F]): F[Map[BlockHash, Validator]] =
    for {
      ib <- dag.invalidBlocks
      r  = ib.map(block => (block.blockHash, block.sender)).toMap
    } yield r

  def selfJustificationChain(h: BlockHash)(implicit sync: Sync[F]): Stream[F, Justification] =
    Stream.unfoldEval(h)(
      message =>
        lookupUnsafe(message)
          .map { v =>
            v.justifications.find(_.validator == v.sender)
          }
          .map(_.map(next => (next, next.latestBlockHash)))
    )

  def selfJustification(h: BlockHash)(implicit sync: Sync[F]): F[Option[Justification]] =
    selfJustificationChain(h).head.compile.last

  def mainParentChain(h: BlockHash, stopAtHeight: Long = 0)(
      implicit sync: Sync[F]
  ): Stream[F, BlockHash] =
    Stream.unfoldEval(h) { message =>
      lookupUnsafe(message).map(
        meta =>
          if (meta.blockNum <= stopAtHeight)
            none[(BlockHash, BlockHash)]
          else
            meta.parents.headOption.map(v => (v, v))
      )
    }

  def isInMainChain(ancestor: BlockHash, descendant: BlockHash)(
      implicit sync: Sync[F]
  ): F[Boolean] = {
    val result = OptionT(dag.lookup(ancestor).map(_.map(_.blockNum))).semiflatMap { aHeight =>
      mainParentChain(descendant, aHeight)
        .filter(_ == ancestor)
        .head
        .compile
        .last
        .map(_.isDefined)
    }
    (descendant == ancestor).pure ||^ result.getOrElse(false)
  }

  def parentsUnsafe(item: BlockHash)(implicit sync: Sync[F]): F[List[BlockHash]] = {
    def errMsg = s"Parents lookup failed: DAG is missing ${item.show}"
    dag.lookup(item).map(_.map(v => v.parents)) >>= (_.liftTo(BlockDagInconsistencyError(errMsg)))
  }

  def nonFinalizedBlocks(implicit sync: Sync[F]): F[Set[BlockHash]] =
    for {
      tips <- latestMessages.map(_.values.map(_.blockHash).toList)
      r <- Stream
            .unfoldLoopEval(tips) { lvl =>
              for {
                out  <- lvl.filterA(dag.isFinalized(_).not)
                next <- out.traverse(dag.lookup(_).map(_.map(_.parents))).map(_.flatten.flatten)
              } yield (out, next.nonEmpty.guard[Option].as(next))
            }
            .flatMap(Stream.emits)
            .compile
            .to(Set)
    } yield r

  def descendants(blockHash: BlockHash)(implicit sync: Sync[F]): F[Set[BlockHash]] =
    Stream
      .unfoldLoopEval(List(blockHash)) { lvl =>
        for {
          out  <- lvl.traverse(dag.children).map(_.flatten.flatten)
          next = out
        } yield (out, next.nonEmpty.guard[Option].as(next))
      }
      .flatMap(Stream.emits)
      .compile
      .to(Set)

  def ancestors(blockHash: BlockHash, filterF: BlockHash => F[Boolean])(
      implicit sync: Sync[F]
  ): F[Set[BlockHash]] =
    Stream
      .unfoldEval(List(blockHash)) { lvl =>
        val parents = lvl
          .traverse(lookupUnsafe)
          .flatMap(_.flatMap(_.parents).distinct.filterA(filterF))
        parents.map(p => p.nonEmpty.guard[Option].as(p, p))
      }
      .flatMap(Stream.emits)
      .compile
      .to(Set)

  def withAncestors(blockHash: BlockHash, filterF: BlockHash => F[Boolean])(
      implicit sync: Sync[F]
  ): F[Set[BlockHash]] =
    ancestors(blockHash, filterF).map(_ + blockHash)
}
