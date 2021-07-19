package com.revdefine.node.web.node

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.api.BlockAPI.BlockRetrievalError
import coop.rchain.casper.engine.EngineCell
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.shared.Log

object BlockApi {

  def getBlock[F[_]: Sync: BlockStore](
      blockHash: String
  ): F[BlockMessage] =
    for {
      hashByteString <- Base16
                         .decode(blockHash)
                         .map(ByteString.copyFrom)
                         .liftTo[F](
                           BlockRetrievalError(
                             s"Input hash value is not valid hex string: $blockHash"
                           )
                         )
      getBlock <- BlockStore[F].getUnsafe(hashByteString)
    } yield getBlock

  def getBlocks[F[_]: Sync: EngineCell: BlockStore](
      depth: Int
  ): F[fs2.Stream[F, BlockMessage]] =
    for {
      engine <- EngineCell[F].read
      blockHashes <- engine.withCasper(
                      casper =>
                        for {
                          dag               <- casper.blockDag
                          latestBlockNumber <- dag.latestBlockNumber
                          blocks            <- dag.topoSort(latestBlockNumber - depth, none)
                          flatBlocks        = blocks.flatten
                        } yield flatBlocks,
                      Sync[F].raiseError(
                        new Exception(
                          s"Casper is not ready in getBlocks request with $depth"
                        )
                      )
                    )
      s      = fs2.Stream.emits(blockHashes)
      blocks = s.evalMap(BlockStore[F].getUnsafe)
    } yield blocks

  def getBlocksByHeights[F[_]: Sync: EngineCell: Log: SafetyOracle: BlockStore](
      startBlockNumber: Long,
      endBlockNumber: Long
  ): F[fs2.Stream[F, BlockMessage]] =
    for {
      engine <- EngineCell[F].read
      blockHashes <- engine.withCasper(
                      casper =>
                        for {
                          dag        <- casper.blockDag
                          blocks     <- dag.topoSort(startBlockNumber, Some(endBlockNumber))
                          flatBlocks = blocks.flatten
                        } yield flatBlocks,
                      Sync[F].raiseError(
                        new Exception(
                          s"Casper is not ready in getBlocksByHeights " +
                            s"request with $startBlockNumber, $endBlockNumber"
                        )
                      )
                    )
      s      = fs2.Stream.emits(blockHashes)
      blocks = s.evalMap(BlockStore[F].getUnsafe)
    } yield blocks
}
