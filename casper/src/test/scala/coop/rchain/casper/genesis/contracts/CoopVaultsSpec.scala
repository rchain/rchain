package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource
import scala.concurrent.duration._

class CoopVaultsSpec
    extends RhoSpec(
      CompiledRholangSource("CoopVaultsTest.rho"),
      Seq(
        StandardDeploys.nonNegativeNumber,
        StandardDeploys.makeMint,
        StandardDeploys.authKey,
        StandardDeploys.either,
        StandardDeploys.revVault
      ),
      3.seconds
    )
