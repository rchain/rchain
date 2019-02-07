package coop.rchain.casper.genesis.contracts
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.rholang.TimeoutResultCollectorTest
import org.scalatest.{AppendedClues, FlatSpec, Matchers}

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

class TimeoutResultCollectorSpec extends FlatSpec with AppendedClues with Matchers {
  it should "testFinished should be false if execution hasn't finished within timeout" in {
    RhoSpec
      .getResults(TimeoutResultCollectorTest, Seq.empty)
      .runSyncUnsafe(Duration.Zero)
      .hasFinished should be(false)
  }
}
