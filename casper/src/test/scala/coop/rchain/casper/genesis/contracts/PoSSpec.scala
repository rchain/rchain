package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource

import scala.concurrent.duration._

class PoSSpec
    extends RhoSpec(
      CompiledRholangSource("PoSTest.rho"),
      Seq.empty,
      GENESIS_TEST_TIMEOUT
    )
