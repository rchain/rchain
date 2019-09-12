package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.ConstructDeploy.defaultSec

import scala.io.Source

class DeployDataContractSpec
    extends RhoSpec(
      Seq((Source.fromResource("DeployDataContractTest.rho").mkString, defaultSec)),
      GENESIS_TEST_TIMEOUT
    )
