package coop.rchain.casper.helper

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.blockstorage.dag.{BlockDagStorage, DagRepresentation}
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{BlockStatus, DeployError, MultiParentCasper, _}
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import coop.rchain.models.blockImplicits.getRandomBlock
import coop.rchain.shared.syntax._

import scala.collection.mutable.{Map => MutableMap}

class NoOpsCasperEffect[F[_]: Sync: BlockStore: BlockDagStorage] private (
    private val store: MutableMap[BlockHash, BlockMessage],
    estimatorFunc: IndexedSeq[BlockHash]
)(implicit runtimeManager: RuntimeManager[F])
    extends MultiParentCasper[F] {

  def addBlock(b: BlockMessage, allowFromStore: Boolean): F[ValidBlockProcessing] =
    for {
      _ <- Sync[F].delay(store.update(b.blockHash, b))
    } yield BlockStatus.valid.asRight
  def contains(blockHash: BlockHash): F[Boolean]       = store.contains(blockHash).pure[F]
  def dagContains(blockHash: BlockHash): F[Boolean]    = false.pure[F]
  def bufferContains(blockHash: BlockHash): F[Boolean] = false.pure[F]
  def deploy(r: Signed[DeployData]): F[Either[DeployError, DeployId]] =
    Applicative[F].pure(Right(ByteString.EMPTY))
  def estimator(dag: DagRepresentation): F[IndexedSeq[BlockHash]] =
    estimatorFunc.pure[F]
  def blockDag: F[DagRepresentation]                                  = BlockDagStorage[F].getRepresentation
  def normalizedInitialFault(weights: Map[Validator, Long]): F[Float] = 0f.pure[F]
  def lastFinalizedBlock: F[BlockMessage]                             = getRandomBlock().pure[F]
  def getRuntimeManager: F[RuntimeManager[F]]                         = runtimeManager.pure[F]
  def fetchDependencies: F[Unit]                                      = ().pure[F]
  def getApprovedBlock: F[BlockMessage]                               = getRandomBlock().pure[F]
  def getValidator: F[Option[ValidatorIdentity]]                      = none[ValidatorIdentity].pure[F]
  def getVersion: F[Long]                                             = 1L.pure[F]
  def getDeployLifespan: F[Int]                                       = Int.MaxValue.pure[F]
  def approvedBlockStateComplete: F[Boolean]                          = true.pure[F]
  def addBlockFromStore(bh: BlockHash, allowFromStore: Boolean): F[ValidBlockProcessing] =
    for {
      b <- BlockStore[F].get1(bh)
      _ <- Sync[F].delay(store.update(b.get.blockHash, b.get))
    } yield BlockStatus.valid.asRight

  override def getSnapshot: F[CasperSnapshot] = ???
  override def validate(
      b: BlockMessage,
      s: CasperSnapshot
  ): F[Either[BlockError, ValidBlock]]                                     = ???
  override def handleValidBlock(block: BlockMessage): F[DagRepresentation] = ???
  override def handleInvalidBlock(
      block: BlockMessage,
      status: InvalidBlock,
      dag: DagRepresentation
  ): F[DagRepresentation]                                         = ???
  override def getDependencyFreeFromBuffer: F[List[BlockMessage]] = ???
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
    apply(Map(ByteString.EMPTY -> getRandomBlock()), Vector(ByteString.EMPTY))
  def apply[F[_]: Sync: BlockStore: BlockDagStorage: RuntimeManager](
      blocks: Map[BlockHash, BlockMessage]
  ): F[NoOpsCasperEffect[F]] =
    apply(blocks, Vector(ByteString.EMPTY))
}
