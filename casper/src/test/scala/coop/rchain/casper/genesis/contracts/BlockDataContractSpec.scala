package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.ConstructDeploy.defaultSec

import scala.concurrent.duration._
import scala.io.Source

class BlockDataContractSpec
    extends RhoSpec(
      Seq((Source.fromResource("BlockDataContractTest.rho").mkString, defaultSec)),
      30.seconds
    )
