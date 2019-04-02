package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource

import scala.concurrent.duration._

class RevVaultSpec
    extends RhoSpec(
      CompiledRholangSource("RevVaultTest.rho"),
      Seq(
        StandardDeploys.nonNegativeNumber,
        StandardDeploys.makeMint,
        StandardDeploys.authKey,
        StandardDeploys.either,
        StandardDeploys.revVault
      ),
      10.seconds
    )
