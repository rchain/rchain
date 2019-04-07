package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource

import scala.concurrent.duration._

class RhoSpecContractSpec
    extends RhoSpec(CompiledRholangSource("RhoSpecContractTest.rho"), Seq.empty, 10.seconds)
