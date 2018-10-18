package coop.rchain.casper.helper

import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, Monad}
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{BlockDag, BlockStatus, CreateBlockStatus, MultiParentCasper}

import scala.collection.mutable.{Map => MutableMap}

class NoOpsCasperEffect[F[_]: Sync: BlockStore] private (
    private val blockStore: MutableMap[BlockHash, BlockMessage],
    estimatorFunc: IndexedSeq[BlockMessage],
    blockDagFunc: BlockDag
) extends MultiParentCasper[F] {

  def store: Map[BlockHash, BlockMessage] = blockStore.toMap

  def addBlock(b: BlockMessage): F[BlockStatus] =
    for {
      _ <- Sync[F].delay(blockStore.update(b.blockHash, b))
      _ <- BlockStore[F].put(b.blockHash, b)
    } yield BlockStatus.valid
  def contains(b: BlockMessage): F[Boolean]             = false.pure[F]
  def deploy(r: DeployData): F[Either[Throwable, Unit]] = Applicative[F].pure(Right(()))
  def estimator(dag: BlockDag): F[IndexedSeq[BlockMessage]] =
    estimatorFunc.pure[F]
  def createBlock: F[CreateBlockStatus]                               = CreateBlockStatus.noNewDeploys.pure[F]
  def blockDag: F[BlockDag]                                           = blockDagFunc.pure[F]
  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] = 0f.pure[F]
  def lastFinalizedBlock: F[BlockMessage]                             = BlockMessage().pure[F]
  def storageContents(hash: BlockHash): F[String]                     = "".pure[F]
  def getRuntimeManager: F[Option[RuntimeManager]]                    = none[RuntimeManager].pure[F]
  def fetchDependencies: F[Unit]                                      = ().pure[F]
}

object NoOpsCasperEffect {
  def apply[F[_]: Sync: BlockStore](
      blockStore: Map[BlockHash, BlockMessage] = Map.empty,
      estimatorFunc: IndexedSeq[BlockMessage] = Vector(BlockMessage()),
      blockDagFunc: BlockDag = BlockDag.empty
  ): F[NoOpsCasperEffect[F]] =
    for {
      _ <- Sync[F].delay { blockStore.map((BlockStore[F].put _).tupled) }
    } yield new NoOpsCasperEffect[F](MutableMap(blockStore.toSeq: _*), estimatorFunc, blockDagFunc)
  def apply[F[_]: Sync: BlockStore](): F[NoOpsCasperEffect[F]] =
    apply(Map.empty, Vector(BlockMessage()), BlockDag.empty)
  def apply[F[_]: Sync: BlockStore](
      blockStore: Map[BlockHash, BlockMessage]
  ): F[NoOpsCasperEffect[F]] =
    apply(blockStore, Vector(BlockMessage()), BlockDag.empty)
}
