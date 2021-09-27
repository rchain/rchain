package coop.rchain.node.blockprocessing

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.engine.BlockRetriever
import coop.rchain.casper.engine.BlockRetriever.MissingDependencyRequested
import coop.rchain.casper.v2.processing.DependenciesRetriever
import coop.rchain.comm.PeerNode
import coop.rchain.models.BlockHash.BlockHash

final case class BlockRetrieverImpl[F[_]: Sync](retriever: BlockRetriever[F])
    extends DependenciesRetriever[F, BlockHash] {
  override def retrieve(messages: Set[BlockHash]): F[Unit] =
    messages.toList.traverse_ { hash =>
      retriever.admitHash(hash, none[PeerNode], MissingDependencyRequested)
    }
  override def ackRetrieved(message: BlockHash): F[Unit] = retriever.ackReceive(message)
}
