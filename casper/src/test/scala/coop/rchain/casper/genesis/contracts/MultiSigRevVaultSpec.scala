package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.models.NormalizerEnv
import coop.rchain.rholang.build.CompiledRholangSource

class MultiSigRevVaultSpec
    extends RhoSpec(
      CompiledRholangSource("MultiSigRevVaultTest.rho", MultiSigRevVaultSpec.normalizerEnv),
      Seq.empty,
      GENESIS_TEST_TIMEOUT
    )

object MultiSigRevVaultSpec {
  val deployerPk    = ConstructDeploy.defaultPub
  val normalizerEnv = NormalizerEnv(deployerPk)
}
