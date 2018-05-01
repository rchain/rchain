package coop.rchain.rosette

import coop.rchain.rosette.Ob.{setLex, SingletonOb}
import org.scalatest.{Matchers, WordSpec}

class ObSpec extends WordSpec with Matchers {
  out =>

  val meta   = new SingletonOb {}
  val parent = new SingletonOb {}

  "forwardingAddress" should {

    "return meta" in {
      val newMeta = createOb()
      val ob      = createOb(m = newMeta)
      ob.forwardingAddress shouldBe newMeta
    }
  }

  "getLex" should {

    "return element by offset" in {
      val third  = createOb()
      val parent = parentWithThreeSlots(third = third)

      val ob  = createOb(p = parent)
      val lex = ob.getLex(indirect = true, level = 1, offset = 2)

      lex shouldEqual third
    }

    "return INVALID when offset is out of bounds" in {
      val parent = parentWithThreeSlots()
      val ob     = createOb(p = parent)
      val lex =
        ob.getLex(indirect = true, level = 1, offset = parent.numberOfSlots + 1)

      lex shouldEqual Ob.INVALID
    }
  }

  "setLex" should {

    "set an extension slot by offset" in {
      val ext    = createExtension(twoSlots)
      val value  = createOb()
      val parent = parentWithThreeSlots(extension = Some(ext))
      val ob     = createOb(m = meta, p = parent, Seq(meta, parent))
      val offset = 1

      val (newOb, res) =
        setLex(indirect = true, level = 1, offset = offset, value = value).run(ob).value

      val actorParent = newOb.parent.asInstanceOf[Actor]

      res shouldEqual Success
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
    val first  = createOb()
    val second = createOb()
    Seq(first, second)
  }

  def createActor(ext: StdExtension, slots: Seq[Ob]): Actor =
    new Actor {
      override val meta          = null
      override val parent        = null
      override val extension     = ext
      override val slot: Seq[Ob] = slots
    }

  def createExtension(slot: Seq[Ob]): StdExtension =
    StdExtension(meta, parent, slot)

  def createOb(m: Ob = out.parent, p: Ob = out.parent, slots: Seq[Ob] = Seq.empty): Ob =
    new Ob {
      override val meta   = m
      override val parent = p
      override val slot   = slots
    }
}
