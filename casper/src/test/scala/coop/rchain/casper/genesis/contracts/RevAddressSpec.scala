package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.rev.RevAddressTest

import scala.concurrent.duration._

class RevAddressSpec extends RhoSpec(RevAddressTest, Seq.empty, 10.seconds)
