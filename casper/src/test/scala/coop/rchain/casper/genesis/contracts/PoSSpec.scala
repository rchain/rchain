package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource

import scala.concurrent.duration._

class PoSSpec
    extends RhoSpec(
      CompiledRholangSource("PoSTest.rho"),
      Seq(
        StandardDeploys.nonNegativeNumber,
        StandardDeploys.makeMint,
        StandardDeploys.authKey,
        StandardDeploys.either,
        StandardDeploys.revVault,
        StandardDeploys.PoS
      ),
      10.seconds
    )
