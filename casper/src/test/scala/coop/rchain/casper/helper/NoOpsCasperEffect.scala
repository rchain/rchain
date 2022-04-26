package coop.rchain.casper.helper

import cats.Applicative
import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.blockStore.BlockStore
import coop.rchain.blockstorage.dag.BlockDagStorage
import coop.rchain.blockstorage.dag.BlockDagStorage.DeployId
import coop.rchain.casper._
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.crypto.signatures.Signed
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.syntax._

import scala.collection.mutable.{Map => MutableMap}

class NoOpsCasperEffect[F[_]: Sync: BlockStore: BlockDagStorage] private (
    private val store: MutableMap[BlockHash, BlockMessage],
    estimatorFunc: IndexedSeq[BlockHash]
)(implicit runtimeManager: RuntimeManager[F])
    extends MultiParentCasper[F] {

  def deploy(r: Signed[DeployData]): F[Either[DeployError, DeployId]] =
    Applicative[F].pure(Right(ByteString.EMPTY))
  def getRuntimeManager: F[RuntimeManager[F]]    = runtimeManager.pure[F]
  def getValidator: F[Option[ValidatorIdentity]] = none[ValidatorIdentity].pure[F]
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
}
