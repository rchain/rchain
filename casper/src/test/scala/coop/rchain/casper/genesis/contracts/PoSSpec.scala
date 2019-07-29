package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.NormalizerEnv
import scala.concurrent.duration._

class PoSSpec
    extends RhoSpec(
      CompiledRholangSource("PoSTest.rho", NormalizerEnv.empty),
      Seq.empty,
      120.seconds
    )
