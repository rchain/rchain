package coop.rchain.casper.genesis.contracts
import cats.implicits._
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.NormalizerEnv

class RevAddressSpec
    extends RhoSpec(
      CompiledRholangSource("RevAddressTest.rho", RevAddressSpec.normalizerEnv),
      GENESIS_TEST_TIMEOUT
    )

object RevAddressSpec {
  val deployerPk    = ConstructDeploy.defaultPub
  val normalizerEnv = NormalizerEnv(none, deployerPk.some)
}
