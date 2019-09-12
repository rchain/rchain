package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.ConstructDeploy.defaultSec
import org.scalatest.{AppendedClues, FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.io.Source

class TimeoutResultCollectorSpec extends FlatSpec with AppendedClues with Matchers {
  it should "testFinished should be false if execution hasn't finished within timeout" in {
    new RhoSpec(
      Seq((Source.fromResource("TimeoutResultCollectorTest.rho").mkString, defaultSec)),
      10.seconds
    ).result.hasFinished should be(false)
  }
}
