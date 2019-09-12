package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.ConstructDeploy.defaultSec

import scala.io.Source

class TreeHashMapSpec
    extends RhoSpec(
      Seq((Source.fromResource("TreeHashMapTest.rho").mkString, defaultSec)),
      GENESIS_TEST_TIMEOUT
    )
