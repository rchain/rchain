package coop.rchain.casper.state.instances

import coop.rchain.casper.blocks.proposer.ProposeResult
import coop.rchain.casper.protocol.BlockMessage
import cats.effect.Deferred

final case class ProposerState[F[_]](
    latestProposeResult: Option[(ProposeResult, Option[BlockMessage])] = None,
    currProposeResult: Option[Deferred[F, (ProposeResult, Option[BlockMessage])]] = None
)
