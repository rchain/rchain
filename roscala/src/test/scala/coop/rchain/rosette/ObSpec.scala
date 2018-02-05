package coop.rchain.rosette

import org.scalatest.{Matchers, WordSpec}

class ObSpec extends WordSpec with Matchers {
  val meta = new Ob {}
  val parent = new Ob {}

  "forwardingAddress" should {

    "return meta" in {
      val newMeta = createOb()
      val ob = createOb(Seq(newMeta))
      ob.forwardingAddress shouldBe newMeta
    }
  }

  "getLex" should {

    "return element by offset" in {
      val third = createOb()
      val parent = parentWithThreeSlots(third)

      val ob = createOb(Seq(meta, parent))
      val lex = ob.getLex(indirect = true, level = 1, offset = 2)

      lex shouldEqual third
    }

    "return INVALID when offset is out of bounds" in {
      val parent = parentWithThreeSlots()
      val ob = createOb(Seq(meta, parent))
      val lex =
        ob.getLex(indirect = true,
                  level = 1,
                  offset = parent.numberOfSlots() + 1)

      lex shouldEqual Ob.INVALID
    }
  }

  "setLex" should {

    "set an extension slot by offset" in {
      val ext = createExtension(twoSlots)
      val value = createOb()
      val parent: Ob = parentWithThreeSlots(extension = Some(ext))
      val ob = createOb(Seq(meta, parent))
      val offset = 1

      val (newOb, newValue) =
        ob.setLex(indirect = true, level = 1, offset = offset, value = value)

      val actorParent = newOb.parent.asInstanceOf[Actor]

      actorParent.extension.slot(offset) shouldEqual value
    }
  }

  def parentWithThreeSlots(third: Ob = createOb(),
                           extension: Option[StdExtension] = None): Actor = {

    val seq = twoSlots :+ third
    val ext = extension.getOrElse(createExtension(slot = seq))
    createActor(ext, seq)
  }

  def twoSlots: Seq[Ob] = {
    val first = createOb()
    val second = createOb()
    Seq(first, second)
  }

  def createActor(ext: StdExtension, slots: Seq[Ob]): Actor =
    new Actor {
      override val extension = ext
      override val slot: Seq[Ob] = slots
    }

  def createExtension(slot: Seq[Ob]): StdExtension =
    StdExtension(meta, parent, slot)

  def createOb(slots: Seq[Ob] = Seq.empty): Ob =
    new Ob {
      override val slot = slots
    }
}
