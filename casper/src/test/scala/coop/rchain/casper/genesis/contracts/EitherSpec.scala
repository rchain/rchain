package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.collection.EitherTest

import scala.concurrent.duration._

class EitherSpec extends RhoSpec(EitherTest, Seq(StandardDeploys.either), 10.seconds)
