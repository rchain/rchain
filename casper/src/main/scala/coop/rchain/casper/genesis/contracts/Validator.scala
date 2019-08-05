package coop.rchain.casper.genesis.contracts

import coop.rchain.crypto.PublicKey
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.{NormalizerEnv, ParBuilder}
import monix.eval.Coeval

final case class Validator(pk: PublicKey, stake: Long)
