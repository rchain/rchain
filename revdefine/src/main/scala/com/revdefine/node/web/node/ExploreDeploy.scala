package com.revdefine.node.web.node

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.api.BlockAPI.BlockRetrievalError
import coop.rchain.casper.engine.EngineCell
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.syntax._
import coop.rchain.casper.util.ProtoUtil
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Par
import coop.rchain.shared.Log

object ExploreDeploy {

  def exploreDeploy[F[_]: Sync: EngineCell: Log: SafetyOracle: BlockStore](
      term: String,
      blockHash: String,
      usePreStateHash: Boolean
  ): F[Seq[Par]] =
    for {
      hashByteString <- Base16
                         .decode(blockHash)
                         .map(ByteString.copyFrom)
                         .liftTo[F](
                           BlockRetrievalError(
                             s"Input hash value is not valid hex string: $blockHash"
                           )
                         )
      block <- BlockStore[F].getUnsafe(hashByteString)
      postStateHash = if (usePreStateHash) ProtoUtil.preStateHash(block)
      else ProtoUtil.postStateHash(block)
      engine <- EngineCell[F].read
      result <- engine.withCasper(
                 casper =>
                   for {
                     runtimeManager <- casper.getRuntimeManager
                     res            <- runtimeManager.playExploratoryDeploy(term, postStateHash)
                   } yield res,
                 Sync[F].raiseError[Seq[Par]](
                   new Exception(
                     s"Casper is not ready on explore deploy request with $term, $blockHash, $usePreStateHash"
                   )
                 )
               )

    } yield result
}
