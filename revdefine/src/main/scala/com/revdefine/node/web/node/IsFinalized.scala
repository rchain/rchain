package com.revdefine.node.web.node

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.api.BlockAPI.BlockRetrievalError
import coop.rchain.casper.engine.EngineCell
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.models.syntax._
import coop.rchain.shared.Log

object IsFinalized {
  def isFinalized[F[_]: Sync: EngineCell: SafetyOracle: BlockStore: Log](
      blockHash: String
  ): F[Boolean] =
    for {
      hashByteString <- blockHash.hexToByteString
                         .liftTo[F](
                           BlockRetrievalError(
                             s"Input hash value is not valid hex string: $blockHash"
                           )
                         )
      engine <- EngineCell[F].read
      result <- engine.withCasper(
                 casper => casper.blockDag >>= (_.isFinalized(hashByteString)),
                 Sync[F].raiseError(
                   new Exception(s"Casper is not ready on isFinalized with $blockHash")
                 )
               )
    } yield result
}
