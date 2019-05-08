package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource
import scala.concurrent.duration._

class LockboxSpec
    extends RhoSpec(
      CompiledRholangSource("LockboxTest.rho"),
      Seq.empty,
      60.seconds
    )
