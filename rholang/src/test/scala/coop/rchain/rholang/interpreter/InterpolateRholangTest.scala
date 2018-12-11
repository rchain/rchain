package coop.rchain.rholang.interpreter

import coop.rchain.models.{Par, Pretty}
import org.scalatest.FlatSpec
import coop.rchain.models.rholang.implicits._
import org.scalactic.TripleEqualsSupport

class InterpolateRholangTest extends FlatSpec with TripleEqualsSupport {

  behavior of "InterpolateRholangTest"

  it should "interpolate" in {
    val rho =
      s"""
         |new x, y in {
         |  @"#x"!(10) | for(@10 <- @"#y") { Nil }
         | }
       """.stripMargin

    val interpolateMap: Map[String, Par] = Map(
      "#x" -> GPrivateBuilder(),
      "#y" -> GPrivateBuilder()
    )

    val interpolated = InterpolateRholang.interpolate(rho, interpolateMap).value
    val send         = interpolated.news.head.p.sends.head
    val receive      = interpolated.news.head.p.receives.head

    assert(send.chan === interpolateMap("#x"))
    assert(receive.binds.head.source === interpolateMap("#y"))
  }

}
