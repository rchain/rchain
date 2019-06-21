package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.NormalizerEnv

class LockboxSpec
    extends RhoSpec(
      CompiledRholangSource("LockboxTest.rho", NormalizerEnv.Empty),
      Seq.empty,
      NormalizerEnv.Empty,
      GENESIS_TEST_TIMEOUT
    )
