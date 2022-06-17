package coop.rchain.casper.genesis.contracts

import coop.rchain.models.NormalizerEnv
import coop.rchain.rholang.build.CompiledRholangTemplate
import coop.rchain.shared.Base16

final case class Registry(systemContractPubKey: String)
    extends CompiledRholangTemplate(
      "Registry.rho",
      NormalizerEnv.Empty,
      "systemContractPubKey" -> systemContractPubKey
    )
