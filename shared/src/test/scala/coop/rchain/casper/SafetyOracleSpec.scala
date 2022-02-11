package coop.rchain.casper

import cats.Id
import cats.syntax.all._
import coop.rchain.casper.pcasper.SafetyOracle
import org.scalatest._

class SafetyOracleSpec extends FlatSpec with Matchers {
  val witnessMap = Map(
    11 -> Map(1 -> 12, 2 -> 22, 3 -> 32),
    21 -> Map(1 -> 12, 2 -> 22, 3 -> 32),
    31 -> Map(1 -> 12, 2 -> 22, 3 -> 32),
    12 -> Map(1 -> 13, 2 -> 23, 3 -> 33),
    22 -> Map(1 -> 13, 2 -> 23, 3 -> 33),
    32 -> Map(1 -> 13, 2 -> 23, 3 -> 33),
    // this is msg of interest
    10 -> Map(1 -> 11, 2 -> 21, 3 -> 31)
  )
  val jsMap = Map(
    11 -> Map(1 -> 0, 2  -> 0, 3  -> 0, 4  -> 0),
    21 -> Map(1 -> 0, 2  -> 0, 3  -> 0, 4  -> 0),
    31 -> Map(1 -> 0, 2  -> 0, 3  -> 0, 4  -> 0),
    12 -> Map(1 -> 11, 2 -> 21, 3 -> 31, 4 -> 0),
    22 -> Map(1 -> 11, 2 -> 21, 3 -> 31, 4 -> 0),
    32 -> Map(1 -> 11, 2 -> 21, 3 -> 31, 4 -> 0),
    13 -> Map(1 -> 12, 2 -> 22, 3 -> 32, 4 -> 0),
    23 -> Map(1 -> 12, 2 -> 22, 3 -> 32, 4 -> 0),
    33 -> Map(1 -> 12, 2 -> 22, 3 -> 32, 4 -> 0),
    // this is msg of interest
    10 -> Map(1 -> 0, 2 -> 0, 3 -> 0, 4 -> 0)
  )
  val sender = (m: Int) => m / 10

  it should "detect valid partition" in {
    val a = SafetyOracle.run[Id, Int, Int](10)(
      witnessMap.getOrElse(_, Map()).pure[Id],
      jsMap.getOrElse(_, Map()).pure[Id]
    )(sender)
    a shouldBe Some(Set(1, 2, 3))
  }

  it should "do not detect partition if justification are changing" in {
    // Here message 32 sees message from sender 4, which should prevent declaration of a partition.
    val jsMap1 = jsMap.updated(32, Map(1 -> 11, 2 -> 21, 3 -> 31, 4 -> 31))
    val a = SafetyOracle.run[Id, Int, Int](10)(
      witnessMap.getOrElse(_, Map()).pure[Id],
      jsMap1.getOrElse(_, Map()).pure[Id]
    )(sender)
    a.isEmpty shouldBe true
  }
}
