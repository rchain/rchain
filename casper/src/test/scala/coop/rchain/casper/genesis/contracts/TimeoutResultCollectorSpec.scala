package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.NormalizerEnv
import org.scalatest.{AppendedClues, FlatSpec, Matchers}

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

class TimeoutResultCollectorSpec extends FlatSpec with AppendedClues with Matchers {
  it should "testFinished should be false if execution hasn't finished within timeout" in {
    new RhoSpec(
      CompiledRholangSource("TimeoutResultCollectorTest.rho", NormalizerEnv.Empty),
      10.seconds
    ).result.hasFinished should be(false)
  }
}
