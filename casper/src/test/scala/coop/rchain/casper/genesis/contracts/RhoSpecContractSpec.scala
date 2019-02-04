package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.RhoSpecContractTest
import scala.concurrent.duration._

class RhoSpecContractSpec
    extends RhoSpec(RhoSpecContractTest, Seq.empty, 10.seconds)
