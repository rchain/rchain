package coop.rchain.roscala

import coop.rchain.roscala.ob._
import coop.rchain.roscala.prim.meta.{addObo, getObo}
import coop.rchain.roscala.util.syntax._
import org.scalatest.{FlatSpec, Matchers}

class MetaSpec extends FlatSpec with Matchers {
  val globalEnv = new GlobalEnv()

  "add-obo" should "add key-value pair to an extensible ob" in {
    val someOb = new Actor()
    someOb.meta = Meta.empty

    val ctxt = Ctxt.argvec(4)
    ctxt.argvec.update(0, someOb.meta)
    ctxt.argvec.update(1, someOb)
    ctxt.argvec.update(2, Symbol("key"))
    ctxt.argvec.update(3, Symbol("value"))

    addObo.fn(ctxt, globalEnv)

    val value = someOb.meta.get(someOb, Symbol("key"), globalEnv)

    value shouldBe Symbol("value")
  }

  "get-obo" should "get the value for a given key from an extensible ob" in {
    val someOb = new Actor()

    someOb.meta = Meta.empty

    // `offset = 0` will point to the first position in `someOb.extension.slot`
    someOb.meta.map(Symbol("key")) = LexVariable(level = 0, offset = 0, indirect = true)
    someOb.extension.slot += Symbol("value")

    val ctxt = Ctxt.argvec(3)
    ctxt.argvec.update(0, someOb.meta)
    ctxt.argvec.update(1, someOb)
    ctxt.argvec.update(2, Symbol("key"))

    val value = getObo.fn(ctxt, globalEnv)

    value shouldBe Symbol("value")
  }
}
