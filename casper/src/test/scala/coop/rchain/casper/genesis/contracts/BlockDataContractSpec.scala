package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.NormalizerEnv

import scala.concurrent.duration._

class BlockDataContractSpec
    extends RhoSpec(
      CompiledRholangSource("BlockDataContractTest.rho", NormalizerEnv.Empty),
      Seq.empty,
      30.seconds
    )
