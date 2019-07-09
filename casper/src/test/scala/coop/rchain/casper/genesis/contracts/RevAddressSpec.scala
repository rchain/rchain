package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.NormalizerEnv

class RevAddressSpec
    extends RhoSpec(
      CompiledRholangSource("RevAddressTest.rho", NormalizerEnv.Empty),
      Seq.empty,
      GENESIS_TEST_TIMEOUT
    )
