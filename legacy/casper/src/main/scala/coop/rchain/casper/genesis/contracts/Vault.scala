package coop.rchain.casper.genesis.contracts
import coop.rchain.rholang.interpreter.util.RevAddress

final case class Vault(revAddress: RevAddress, initialBalance: Long)
