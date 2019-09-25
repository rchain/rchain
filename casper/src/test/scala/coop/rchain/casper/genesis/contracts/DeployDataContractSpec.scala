package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.NormalizerEnv

class DeployDataContractSpec
    extends RhoSpec(
      CompiledRholangSource("DeployDataContractTest.rho", NormalizerEnv.Empty),
      GENESIS_TEST_TIMEOUT
    )
