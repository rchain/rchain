package coop.rchain.rosette

import org.scalatest.{FlatSpec, Matchers, WordSpec}

import scala.collection.mutable

class ObSpec extends WordSpec with Matchers {

  "forwardingAddress" should {

    "return meta" in {
      val newMeta = createOb()
      val ob = createOb(meta = newMeta)
      ob.forwardingAddress shouldBe newMeta
    }
  }

  "self" should {

    "return \"this\"" in {
      val ob = createOb()
      ob.self shouldEqual ob
    }
  }

  "getLex" should {

    "return element by offset" in {
      val third = createOb()
      val parent = parentWithThreeSlots(third)

      val ob = createOb(parent = parent)
      val lex = ob.getLex(ind = 1, level = 1, offset = 2)

      lex shouldEqual third
    }

    "return INVALID when offset is out of bounds" in {
      val parent = parentWithThreeSlots()
      val ob = createOb(parent = parent)
      val lex =
        ob.getLex(ind = 1, level = 1, offset = parent.numberOfSlots() + 1)

      lex shouldEqual Ob.INVALID
    }
  }

  "setLex" should {

    "set an extension slot by offset" in {
      val ext = createOb(slot = twoSlots)
      val value = createOb()
      val parent: Ob = parentWithThreeSlots(extension = Some(ext))
      val ob = createOb(parent = parent)
      val offset = 1
      ob.setLex(ind = 1, level = 1, offset = offset, value = value)
      ext.slot(offset) shouldEqual value
    }
  }

  def parentWithThreeSlots(third: Ob = null,
                           extension: Option[Ob] = None): Actor = {

    val seq = twoSlots :+ third
    val ext = extension.getOrElse(createOb(slot = seq))
    createActor(ext, seq)
  }

  def twoSlots: mutable.Seq[Ob] = {
    val first = createOb()
    val second = createOb()
    mutable.Seq(first, second)
  }

  def createActor(ext: Ob, slots: mutable.Seq[Ob]): Actor =
    new Actor {
      override val extension = ext
      override val _slot = null +: null +: slots
    }

  def createOb(meta: Ob = null,
               parent: Ob = null,
               slot: mutable.Seq[Ob] = mutable.Seq.empty): Ob = {
    val (m, p, s) = (meta, parent, slot)
    new Ob {
      override val _slot = m +: p +: s
    }
  }
}
