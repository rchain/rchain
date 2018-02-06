package coop.rchain.rosette.prim

import coop.rchain.rosette.prim.tuple._
import coop.rchain.rosette.{Ctxt, Fixnum => RFixnum, Ob, PC, RblBool, Tuple}
import org.scalatest._

class TupleSpec extends FlatSpec with Matchers {
  val ctxt = Ctxt(
    tag = null,
    nargs = 1,
    outstanding = 0,
    pc = PC.PLACEHOLDER,
    rslt = null,
    trgt = null,
    argvec = Tuple(1, RFixnum(1)),
    env = null,
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null
  )

  "tplCons" should "correctly cons an Ob with a Tuple" in {
    val test = Seq(RFixnum(2), RFixnum(3), RFixnum(4))
    val newCtxt =
      ctxt.copy(nargs = 2, argvec = Tuple.cons(RFixnum(1), Tuple(Tuple(test))))
    tplCons.fn(newCtxt) should be(Right(Tuple.cons(RFixnum(1), Tuple(test))))
  }

  it should "fail for non-tuple arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    tplCons.fn(newCtxt) should be('left)
  }

}
