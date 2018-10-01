package coop.rchain.models

import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._

import org.scalatest.{Assertion, FlatSpec, Matchers}

class IsForgeableSpec extends FlatSpec with Matchers {

  "A GPrivate" should "not be forgeable" in {
    val unforgeable: Par = GPrivateBuilder("unforgeable")

    ParIsForgeable(unforgeable) should be(false)
  }
  "A ground type" should "be forgeable" in {
    val unforgeable: Par = GString("forgeable")

    ParIsForgeable(unforgeable) should be(true)
  }
  "A send of a ground type on a ground type" should "be forgeable" in {
    val unforgeable: Par = Send(GString("forge1"), List(GString("forge2")))

    ParIsForgeable(unforgeable) should be(true)
  }
  "A send of a ground type on an unforgeable name" should "be unforgeable" in {
    val unforgeable: Par = Send(GPrivateBuilder("unforge"), List(GString("forge2")))

    ParIsForgeable(unforgeable) should be(false)
  }
  "A send of an unforgeable on a ground type" should "be unforgeable" in {
    val unforgeable: Par = Send(GString("forge1"), List(GPrivateBuilder("unforge")))

    ParIsForgeable(unforgeable) should be(false)
  }
}
