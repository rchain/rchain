package com.revdefine.node.web.node

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.casper.engine.EngineCell
import coop.rchain.casper.engine.EngineCell.EngineCell
import coop.rchain.casper.protocol.BlockMessage

object LastFinalizedBlock {
  def lastFinalizedBlock[F[_]: Sync: EngineCell]: F[BlockMessage] =
    EngineCell[F].read >>= (
      _.withCasper(
        casper => casper.lastFinalizedBlock,
        Sync[F].raiseError(new Exception("Casper is not ready on last finalized block request."))
      )
    )
}
