package coop.rchain.casper.genesis.contracts

import cats.implicits._
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.rholang.build.CompiledRholangSource

import coop.rchain.rholang.interpreter.NormalizerEnv

class RevVaultSpec
    extends RhoSpec(
      CompiledRholangSource("RevVaultTest.rho", RevVaultSpec.normalizerEnv),
      Seq.empty,
      GENESIS_TEST_TIMEOUT
    )

object RevVaultSpec {
  val deployerPk    = ConstructDeploy.defaultPub
  val normalizerEnv = NormalizerEnv(none, deployerPk.some)
}
