package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.models.NormalizerEnv
import coop.rchain.rholang.build.CompiledRholangSource

class PubKeyCheckSpec
    extends RhoSpec(
      CompiledRholangSource("PubKeyCheckTest.rho", NormalizerEnv.Empty),
      Seq.empty,
      GENESIS_TEST_TIMEOUT
    )
