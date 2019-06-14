package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource
import scala.concurrent.duration._

class BlockDataContractSpec
    extends RhoSpec(CompiledRholangSource("BlockDataContractTest.rho"), Seq.empty, 30.seconds)
