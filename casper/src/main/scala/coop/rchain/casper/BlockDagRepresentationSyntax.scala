package coop.rchain.casper

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.blockstorage.dag.BlockDagRepresentation
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.protocol.Justification
import coop.rchain.casper.safety.CliqueOracle
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator

trait BlockDagRepresentationSyntax {
  implicit final def casperSyntaxBlockDagRepresentation[F[_]](
      dag: BlockDagRepresentation[F]
  ): BlockDagRepresentationOps[F] = new BlockDagRepresentationOps[F](dag)
}

final class BlockDagRepresentationOps[F[_]](
    private val dag: BlockDagRepresentation[F]
) extends AnyVal {
  def getCasperJustificationsUnsafe(
      blockHash: BlockHash
  )(implicit sync: Sync[F]): F[Set[CliqueOracle.Justification[BlockHash, Validator]]] =
    dag
      .lookupUnsafe(blockHash)
      .map(
        _.justifications.map {
          case Justification(validator, latestBlockHash) =>
            CliqueOracle.Justification(latestBlockHash, validator)
        }.toSet
      )

  def toCasperJustificationUnsafe(
      blockHash: BlockHash
  )(implicit sync: Sync[F]): F[CliqueOracle.Justification[BlockHash, Validator]] =
    dag.lookupUnsafe(blockHash).map(m => CliqueOracle.Justification(m.blockHash, m.sender))
}
