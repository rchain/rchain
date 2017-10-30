package coop.rchain.rosette

import org.scalatest.{FlatSpec, Matchers}

class ObSpec extends FlatSpec with Matchers {

  "forwardingAddress" should "return meta" in {
    val newMeta = createOb()
    val ob = createOb(meta = newMeta)
    ob.forwardingAddress shouldBe newMeta
  }

  "self" should "return \"this\"" in {
    val ob = createOb()
    ob.self shouldEqual ob
  }

  def createOb(meta: Ob = null, parent: Ob = null, slot: Seq[Ob] = null): Ob = {
    val values = (meta, parent, slot)
    new Ob {
      override val meta = values._1
      override val parent = values._2
      override val slot = values._3
    }
  }
}
