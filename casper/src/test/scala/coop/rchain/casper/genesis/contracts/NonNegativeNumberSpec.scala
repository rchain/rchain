package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.ConstructDeploy.defaultSec

import scala.io.Source

class NonNegativeNumberSpec
    extends RhoSpec(
      Seq((Source.fromResource("NonNegativeNumberTest.rho").mkString, defaultSec)),
      GENESIS_TEST_TIMEOUT
    )
