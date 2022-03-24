package coop.rchain.sdk.block.state
import coop.rchain.sdk.block.state.Buffered.{AckValidatedEffect, BufferedEffect}
import coop.rchain.sdk.block.state.Requested.{AckBufferedEffect, RequestedEffect}
import coop.rchain.sdk.block.state.Validated.Offence

/** State of all blocks known. */
final case class BlocksState[BId, Peer](
    requested: Requested[BId, Peer],
    buffer: Buffered[BId],
    validated: Validated[BId]
) {
  type ST = BlocksState[BId, Peer]

  // New block id is received
  def bidReceived: (ST, RequestedEffect[Peer]) = ???
  // Full block is received
  def blockReceived: (ST, AckBufferedEffect, BufferedEffect[BId]) = ???
  // Block has been validated
  def blockValidated(bid: BId, offenceOpt: Option[Offence]): (ST, AckValidatedEffect[BId]) = ???
}
