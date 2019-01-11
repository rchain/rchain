package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.mint.MakeMintTest

import scala.concurrent.duration._

class MakeMintSpec
    extends RhoSpec(
      MakeMintTest,
      Seq(
        StandardDeploys.nonNegativeNumber,
        StandardDeploys.makeMint
      ),
      10.seconds
    )
