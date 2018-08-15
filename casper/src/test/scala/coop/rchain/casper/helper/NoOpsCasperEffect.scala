package coop.rchain.casper.helper

import cats.{Applicative, Id, Monad}
import cats.implicits._
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.Estimator.{BlockHash, Validator}
import coop.rchain.casper.{BlockDag, BlockStatus, MultiParentCasper}
import coop.rchain.casper.protocol.{BlockMessage, Deploy, DeployData}
import coop.rchain.casper.util.rholang.RuntimeManager

object NoOpsCasperEffect {
  def testCasper[F[_]: Monad: BlockStore](
      blockStore: Map[BlockHash, BlockMessage],
      estimatorFunc: F[IndexedSeq[BlockMessage]] = Vector(BlockMessage()),
      blockDagFunc: F[BlockDag] = BlockDag()): MultiParentCasper[F] =
    new MultiParentCasper[F] {

      blockStore.map {
        case (hash, block) =>
          BlockStore[F].put(hash, block)
      }

      def addBlock(b: BlockMessage): F[BlockStatus]                      = BlockStatus.valid.pure[F]
      def contains(b: BlockMessage): F[Boolean]                          = false.pure[F]
      def deploy(r: DeployData): F[Either[Throwable, Unit]]              = Applicative[F].pure(Right(()))
      def estimator: F[IndexedSeq[BlockMessage]]                         = estimatorFunc
      def createBlock: F[Option[BlockMessage]]                           = Applicative[F].pure[Option[BlockMessage]](None)
      def blockDag: F[BlockDag]                                          = blockDagFunc
      def normalizedInitialFault(weights: Map[Validator, Int]): F[Float] = 0f.pure[F]
      def lastFinalizedBlock: F[BlockMessage]                            = BlockMessage().pure[F]
      def storageContents(hash: BlockHash): F[String]                    = "".pure[F]
      def getRuntimeManager: F[Option[RuntimeManager]]                   = none[RuntimeManager].pure[F]
    }
}
