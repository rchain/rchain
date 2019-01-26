package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.wallet.RevTest

import scala.concurrent.duration._

class RevSpec
    extends RhoSpec(
      RevTest,
      Seq(
        StandardDeploys.nonNegativeNumber,
        StandardDeploys.makeMint,
        StandardDeploys.basicWallet,
        StandardDeploys.walletCheck,
        StandardDeploys.makeRev
      ),
      10.seconds
    )
