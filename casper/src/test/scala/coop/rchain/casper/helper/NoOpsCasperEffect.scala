package coop.rchain.casper.helper

import cats.effect.{Resource, Sync}
import cats.implicits._
import cats.{Applicative, Monad}
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.dag.{BlockDagRepresentation, BlockDagStorage}
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper._
import coop.rchain.casper.DeployError
import coop.rchain.casper.protocol.{BlockMessage, DeployData, Dummies}
import coop.rchain.casper.util.rholang.Resources.mkRuntimeManager
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{BlockStatus, CreateBlockStatus, MultiParentCasper}
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import monix.eval.Task

import scala.collection.mutable.{Map => MutableMap}

class NoOpsCasperEffect[F[_]: Sync: BlockStore: BlockDagStorage] private (
    private val blockStore: MutableMap[BlockHash, BlockMessage],
    estimatorFunc: IndexedSeq[BlockHash]
)(implicit runtimeManager: RuntimeManager[F])
    extends MultiParentCasper[F] {

  def store: Map[BlockHash, BlockMessage] = blockStore.toMap

  def addBlock(
      b: BlockMessage,
      handleDoppelganger: (BlockMessage, Validator) => F[Unit]
  ): F[ValidBlockProcessing] =
    for {
      _ <- Sync[F].delay(blockStore.update(b.blockHash, b))
      _ <- BlockStore[F].put(b.blockHash, b)
    } yield BlockStatus.valid.asRight
  def contains(blockHash: BlockHash): F[Boolean] = false.pure[F]
  def deploy(r: DeployData): F[Either[DeployError, DeployId]] =
    Applicative[F].pure(Right(ByteString.EMPTY))
  def estimator(dag: BlockDagRepresentation[F]): F[IndexedSeq[BlockHash]] =
    estimatorFunc.pure[F]
  def createBlock: F[CreateBlockStatus]                               = CreateBlockStatus.noNewDeploys.pure[F]
  def blockDag: F[BlockDagRepresentation[F]]                          = BlockDagStorage[F].getRepresentation
  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] = 0f.pure[F]
  def lastFinalizedBlock: F[BlockMessage]                             = Dummies.createBlockMessage().pure[F]
  def getRuntimeManager: F[RuntimeManager[F]]                         = runtimeManager.pure[F]
  def fetchDependencies: F[Unit]                                      = ().pure[F]
}

object NoOpsCasperEffect {
  def apply[F[_]: Sync: BlockStore: BlockDagStorage: RuntimeManager](
      blocks: Map[BlockHash, BlockMessage] = Map.empty,
      estimatorFunc: IndexedSeq[BlockHash] = Vector(ByteString.EMPTY)
  ): F[NoOpsCasperEffect[F]] =
    for {
      _ <- blocks.toList.traverse_ {
            case (blockHash, block) => BlockStore[F].put(blockHash, block)
          }
    } yield new NoOpsCasperEffect[F](MutableMap(blocks.toSeq: _*), estimatorFunc)
  def apply[F[_]: Sync: BlockStore: BlockDagStorage: RuntimeManager](): F[NoOpsCasperEffect[F]] =
    apply(Map(ByteString.EMPTY -> Dummies.createBlockMessage()), Vector(ByteString.EMPTY))
  def apply[F[_]: Sync: BlockStore: BlockDagStorage: RuntimeManager](
      blocks: Map[BlockHash, BlockMessage]
  ): F[NoOpsCasperEffect[F]] =
    apply(blocks, Vector(ByteString.EMPTY))
}
