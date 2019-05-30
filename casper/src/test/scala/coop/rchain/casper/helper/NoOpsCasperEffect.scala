package coop.rchain.casper.helper

import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.{BlockDagRepresentation, BlockDagStorage, BlockStore}
import coop.rchain.casper._
import coop.rchain.casper.DeployError
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{BlockStatus, CreateBlockStatus, MultiParentCasper}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import monix.eval.Task

import scala.collection.mutable.{Map => MutableMap}

class NoOpsCasperEffect[F[_]: Sync: BlockStore: BlockDagStorage] private (
    private val blockStore: MutableMap[BlockHash, BlockMessage],
    estimatorFunc: IndexedSeq[BlockHash]
) extends MultiParentCasper[F] {

  def store: Map[BlockHash, BlockMessage] = blockStore.toMap

  def addBlock(
      b: BlockMessage,
      handleDoppelganger: (BlockMessage, Validator) => F[Unit]
  ): F[BlockStatus] =
    for {
      _ <- Sync[F].delay(blockStore.update(b.blockHash, b))
      _ <- BlockStore[F].put(b.blockHash, b)
    } yield BlockStatus.valid
  def contains(b: BlockMessage): F[Boolean] = false.pure[F]
  def deploy(r: DeployData): F[Either[DeployError, DeployId]] =
    Applicative[F].pure(Right(Array[Byte]()))
  def estimator(dag: BlockDagRepresentation[F]): F[IndexedSeq[BlockHash]] =
    estimatorFunc.pure[F]
  def createBlock: F[CreateBlockStatus]                               = CreateBlockStatus.noNewDeploys.pure[F]
  def blockDag: F[BlockDagRepresentation[F]]                          = BlockDagStorage[F].getRepresentation
  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] = 0f.pure[F]
  def lastFinalizedBlock: F[BlockMessage]                             = BlockMessage().pure[F]
  def storageContents(hash: BlockHash): F[String]                     = "".pure[F]
  def getRuntimeManager: F[Option[RuntimeManager[F]]]                 = none[RuntimeManager[F]].pure[F]
  def fetchDependencies: F[Unit]                                      = ().pure[F]
}

object NoOpsCasperEffect {
  def apply[F[_]: Sync: BlockStore: BlockDagStorage](
      blocks: Map[BlockHash, BlockMessage] = Map.empty,
      estimatorFunc: IndexedSeq[BlockHash] = Vector(BlockMessage().blockHash)
  ): F[NoOpsCasperEffect[F]] =
    for {
      _ <- blocks.toList.traverse_ {
            case (blockHash, block) => BlockStore[F].put(blockHash, block)
          }
    } yield new NoOpsCasperEffect[F](MutableMap(blocks.toSeq: _*), estimatorFunc)
  def apply[F[_]: Sync: BlockStore: BlockDagStorage](): F[NoOpsCasperEffect[F]] =
    apply(Map(BlockMessage().blockHash -> BlockMessage()), Vector(BlockMessage().blockHash))
  def apply[F[_]: Sync: BlockStore: BlockDagStorage](
      blocks: Map[BlockHash, BlockMessage]
  ): F[NoOpsCasperEffect[F]] =
    apply(blocks, Vector(BlockMessage().blockHash))
}
