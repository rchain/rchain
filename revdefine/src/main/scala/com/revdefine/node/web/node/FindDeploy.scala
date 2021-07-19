package com.revdefine.node.web.node

import cats.effect.Sync
import cats.syntax.all._
import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.syntax._
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.engine.EngineCell
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.codec.Base16
import coop.rchain.shared.Log

object FindDeploy {

  def findDeploy[F[_]: Sync: EngineCell: Log: SafetyOracle: BlockStore](
      deployId: String,
      oldDeployMap: Map[String, String]
  ): F[BlockMessage] = {
    val oldDeployOpt = oldDeployMap.get(deployId)
    for {
      blockHash <- if (oldDeployOpt.isEmpty) for {
                    hashByteString <- Base16
                                       .decode(deployId)
                                       .map(ByteString.copyFrom)
                                       .liftTo[F](
                                         new Exception(
                                           s"Input hash value is not valid hex string: $deployId"
                                         )
                                       )
                    engine <- EngineCell[F].read
                    blockHash <- engine.withCasper(
                                  casper =>
                                    for {
                                      dag            <- casper.blockDag
                                      maybeBlockHash <- dag.lookupByDeployId(hashByteString)
                                      blockHash <- maybeBlockHash.liftTo(
                                                    new Exception(
                                                      s"Can not find deploy for deployID : $deployId"
                                                    )
                                                  )
                                    } yield blockHash,
                                  Sync[F].raiseError(
                                    new Exception(
                                      s"Casper is not ready in findDeploy with ${deployId}"
                                    )
                                  )
                                )
                  } yield blockHash
                  else
                    Base16
                      .decode(oldDeployOpt.get)
                      .map(ByteString.copyFrom)
                      .liftTo[F](
                        new Exception(
                          s"Input hash value is not valid hex string on old deploy map: $oldDeployOpt.get.head"
                        )
                      )
      block <- BlockStore[F].getUnsafe(blockHash)
    } yield block
  }
}
