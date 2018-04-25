package coop.rchain.rosette.prim

import coop.rchain.rosette.prim.ob._
import coop.rchain.rosette.{Ctxt, Fixnum, Ob, PC, RblBool, RblString, RblSymbol, Tuple}
import org.scalatest._

class ObSpec extends FlatSpec with Matchers {
  val ctxt = Ctxt(
    tag = null,
    nargs = 1,
    outstanding = 0,
    pc = PC.PLACEHOLDER,
    rslt = null,
    trgt = null,
    argvec = Tuple(1, Fixnum(1)),
    env = null,
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null,
  )

  "objectToString" should "convert an object into its string representation" in {
    val newCtxt = ctxt.copy(argvec = Tuple(1, Fixnum(5)))
    objectToString.fnSimple(newCtxt) should be(Right(RblString("Fixnum(5)")))
  }

  "objectToString" should "convert multiple objects into their string representations" in {
    val args    = Seq(Fixnum(5), RblBool(true))
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(args))
    objectToString.fnSimple(newCtxt) should be(Right(RblString("Fixnum(5)RblBool(true)")))
  }

  "objectToSymbol" should "convert an object into its symbol representation" in {
    val newCtxt = ctxt.copy(argvec = Tuple(1, Fixnum(5)))
    objectToSymbol.fnSimple(newCtxt) should be(Right(RblSymbol(Symbol("Fixnum(5)"))))
  }

  "objectToSymbol" should "convert multiple objects into their symbol representations" in {
    val args    = Seq(Fixnum(5), RblBool(true))
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(args))
    objectToSymbol.fnSimple(newCtxt) should be(Right(RblSymbol(Symbol("Fixnum(5)RblBool(true)"))))
  }
}
