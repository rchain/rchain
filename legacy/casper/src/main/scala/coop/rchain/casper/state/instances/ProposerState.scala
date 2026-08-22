package coop.rchain.casper.state.instances

import cats.effect.concurrent.Deferred
import coop.rchain.casper.blocks.proposer.ProposeResult
import coop.rchain.casper.protocol.BlockMessage

final case class ProposerState[F[_]](
    latestProposeResult: Option[(ProposeResult, Option[BlockMessage])] = None,
    currProposeResult: Option[Deferred[F, (ProposeResult, Option[BlockMessage])]] = None
)
