package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.NormalizerEnv

class MakeMintSpec
    extends RhoSpec(
      CompiledRholangSource("MakeMintTest.rho", NormalizerEnv.Empty),
      Seq.empty,
      GENESIS_TEST_TIMEOUT
    )
