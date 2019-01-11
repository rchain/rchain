package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.math.NonNegativeNumberTest

import scala.concurrent.duration._

class NonNegativeNumberSpec
    extends RhoSpec(NonNegativeNumberTest, Seq(StandardDeploys.nonNegativeNumber), 10.seconds)
