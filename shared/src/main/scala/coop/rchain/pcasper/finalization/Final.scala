package coop.rchain.pcasper.finalization

/**
  * View on finalization of some Casper message.
  */
trait Final[M, S] {
  type Provisional
  // Complete fringe across all senders in the shard.
  def complete: Fringe[M, S]
  // The exact type for representation of provisional depends on implementation
  def provisional: Provisional
}
