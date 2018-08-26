package coop.rchain.casper.helper

import cats.implicits._
import cats.{Applicative, Monad}
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.protocol.{BlockMessage, DeployData}
import coop.rchain.casper.util.rholang.RuntimeManager
import coop.rchain.casper.{BlockDag, BlockStatus, MultiParentCasper}

import scala.collection.mutable.{Map => MutableMap}

class NoOpsCasperEffect[F[_]: Monad](blockStore: Map[BlockHash, BlockMessage] = Map.empty,
                                     estimatorFunc: IndexedSeq[BlockMessage] = Vector(
                                       BlockMessage()),
                                     blockDagFunc: BlockDag = BlockDag())
    extends MultiParentCasper[F] {

  val store: MutableMap[BlockHash, BlockMessage] = MutableMap(blockStore.toSeq: _*)

  def addBlock(b: BlockMessage): F[BlockStatus] = {
    store.update(b.blockHash, b)
    BlockStatus.valid.pure[F]
  }
  def contains(b: BlockMessage): F[Boolean]             = false.pure[F]
  def deploy(r: DeployData): F[Either[Throwable, Unit]] = Applicative[F].pure(Right(()))
  def estimator: F[IndexedSeq[BlockMessage]] =
    estimatorFunc.pure[F]
  def createBlock: F[Option[BlockMessage]]                           = Applicative[F].pure[Option[BlockMessage]](None)
  def blockDag: F[BlockDag]                                          = blockDagFunc.pure[F]
  def normalizedInitialFault(weights: Map[Validator, Int]): F[Float] = 0f.pure[F]
  def lastFinalizedBlock: F[BlockMessage]                            = BlockMessage().pure[F]
  def storageContents(hash: BlockHash): F[String]                    = "".pure[F]
  def getRuntimeManager: F[Option[RuntimeManager]]                   = none[RuntimeManager].pure[F]
}
