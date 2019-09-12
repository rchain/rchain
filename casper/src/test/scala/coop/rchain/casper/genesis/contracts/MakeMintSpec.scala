package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.ConstructDeploy.defaultSec

import scala.io.Source

class MakeMintSpec
    extends RhoSpec(
      Seq((Source.fromResource("MakeMintTest.rho").mkString, defaultSec)),
      GENESIS_TEST_TIMEOUT
    )
