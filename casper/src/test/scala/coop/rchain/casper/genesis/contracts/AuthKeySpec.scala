package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource

class AuthKeySpec
    extends RhoSpec(
      CompiledRholangSource("AuthKeyTest.rho"),
      Seq.empty,
      GENESIS_TEST_TIMEOUT
    )
