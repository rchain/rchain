package coop.rchain.node.casper
import coop.rchain.rspace.hashing.Blake2b256Hash

/**
 * State client that follows part of the global state, defined by set of roots in History.
 */
case class LightClient(roots: Set[Blake2b256Hash])

/**
 * State client maintaining the whole state of the blockchain is Validator.
 */
case object Validator extends LightClient(Set.empty)
