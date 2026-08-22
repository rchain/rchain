package coop.rchain.casper.genesis.contracts

import coop.rchain.crypto.PublicKey

final case class Validator(pk: PublicKey, stake: Long)
