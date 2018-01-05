package coop.rchain.rosette

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class PrimPropertySpec extends PropSpec with PropertyChecks with Matchers {
  property("Primitives should return error for illegal number of arguments") {
    val nargsGen = Gen.choose(0, 1000)
    val primitives = prim.Prims.values

    primitives.foreach { prim =>
      forAll(nargsGen) { i: Int =>
        whenever(i < prim.minArgs || i > prim.maxArgs) {
          val ctxt =
            utils.opcodes.ctxt.copy(nargs = i, argvec = Tuple(i, Fixnum(1)))

          prim.fn(ctxt) should be('left)
        }
      }
    }
  }
}
