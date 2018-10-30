package coop.rchain.models

import coop.rchain.models.Expr.ExprInstance.{GInt, GString}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import org.scalatest.{FlatSpec, Matchers}
import coop.rchain.models.rholang.implicits._
import org.scalactic.TripleEqualsSupport

class InterpolateSpec extends FlatSpec with TripleEqualsSupport with Matchers {

  "Interpolating Send" should "replace channel with the key from the interpolate map" in {
    val send             = Send(GString("#key1"), Seq(GInt(1), GInt(2)))
    val unforgeable: Par = GPrivateBuilder()
    val interpolateMap   = Map(("#key1", unforgeable))
    val interpolated     = Interpolate.interpolate(send, interpolateMap)
    assert(interpolated === send.withChan(unforgeable))
  }

  it should "not replace channel which is not to be substituted" in {
    val send           = Send(GString("key1"), Seq(GInt(1), GInt(2)))
    val interpolateMap = Map.empty[String, Par]
    val interpolated   = Interpolate.interpolate(send, interpolateMap)
    assert(interpolated === send)
  }

  it should "replace data sent" in {
    val fixedElement: Par = GInt(10)
    val send              = Send(GPrivateBuilder(), Seq(GString("#key1"), GString("#key2"), fixedElement))
    val interpolateMap: Map[String, Par] = Map(
      "#key1" -> GPrivateBuilder(),
      "#key2" -> GPrivateBuilder()
    )
    val newData: Seq[Par] = interpolateMap.values.toSeq :+ fixedElement

    val interpolated = Interpolate.interpolate(send, interpolateMap)
    assert(interpolated === send.withData(newData))
  }

  "Interpolating Receive" should "replace source channel with the key from the interpolate map" in {
    val interpolateMap: Map[String, Par] = Map(
      "#key1" -> GPrivateBuilder(),
      "#key2" -> GPrivateBuilder()
    )

    val binds = interpolateMap.keys.zipWithIndex.map {
      case (key, idx) =>
        ReceiveBind(Seq(EVar(Var(FreeVar(idx)))), GString(key), freeCount = 1)
    }.toSeq

    val interpolatedBinds = interpolateMap.values.zipWithIndex.map {
      case (v, idx) =>
        ReceiveBind(Seq(EVar(Var(FreeVar(idx)))), v, freeCount = 1)
    }.toSeq

    val receive      = Receive(binds, Par())
    val interpolated = Interpolate.interpolate(receive, interpolateMap)
    assert(interpolated === receive.withBinds(interpolatedBinds))
  }

  "Interpolating New" should "interpolated in the body" in {
    // copied from the receive test
    val interpolateMap: Map[String, Par] = Map(
      "#key1" -> GPrivateBuilder(),
      "#key2" -> GPrivateBuilder()
    )

    val binds = interpolateMap.keys.zipWithIndex.map {
      case (key, idx) =>
        ReceiveBind(Seq(EVar(Var(FreeVar(idx)))), GString(key), freeCount = 1)
    }.toSeq

    val receive             = Receive(binds, Par())
    val interpolatedReceive = Interpolate.interpolate(receive, interpolateMap)

    val neu          = New(bindCount = 2, receive)
    val interpolated = Interpolate.interpolate(neu, interpolateMap)
    assert(interpolated === neu.withP(interpolatedReceive))
  }

  "Interpolate" should "throw exception when key is not found in the interpolation map" in {
    val send           = Send(GString("#key1"), Seq(GInt(1), GInt(2)))
    val interpolateMap = Map.empty[String, Par]
    a[IllegalArgumentException] shouldBe thrownBy {
      Interpolate.interpolate(send, interpolateMap)
    }
  }
}
