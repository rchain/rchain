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

  /** tuple-cons */
  "tplCons" should "correctly cons an Ob with a Tuple" in {
    val tup = Seq(RFixnum(2), RFixnum(3), RFixnum(4))
    val newCtxt =
      ctxt.copy(nargs = 2, argvec = Tuple.cons(RFixnum(1), Tuple(Tuple(tup))))
    tplCons.fn(newCtxt) should be(Right(Tuple.cons(RFixnum(1), Tuple(tup))))
  }

  it should "fail for non-tuple arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    tplCons.fn(newCtxt) should be('left)
  }

  /** tuple-cons* */
  "tplConsStar" should "correctly cons n Obs with a Tuple" in {
    val tup = Seq(RFixnum(4), RFixnum(5), RFixnum(6))

    val newCtxt =
      ctxt.copy(nargs = 4,
                argvec =
                  Tuple.cons(RFixnum(1),
                             Tuple.cons(RFixnum(2), Tuple.cons(RFixnum(3), Tuple(Tuple(tup))))))

    tplConsStar.fn(newCtxt) should be(
      Right(Tuple.cons(RFixnum(1), Tuple.cons(RFixnum(2), Tuple.cons(RFixnum(3), Tuple(tup))))))
  }

  "tplConsStar" should "correctly cons 0 Obs with a Tuple" in {
    val tup = Seq(RFixnum(4), RFixnum(5), RFixnum(6))

    val newCtxt =
      ctxt.copy(nargs = 1, argvec = Tuple(Tuple(tup)))

    tplConsStar.fn(newCtxt) should be(Right(Tuple(tup)))
  }

  it should "fail for non-tuple arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    tplCons.fn(newCtxt) should be('left)
  }

  /** tuple-rcons */
  "tplRcons" should "correctly rcons an Ob with a Tuple" in {
    val tup = Seq(RFixnum(1), RFixnum(2), RFixnum(3))
    val newCtxt =
      ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(Tuple(tup)), RFixnum(4)))
    tplRcons.fn(newCtxt) should be(Right(Tuple.rcons(Tuple(tup), RFixnum(4))))
  }

  it should "fail for non-tuple arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    tplRcons.fn(newCtxt) should be('left)
  }

  /** tuple-concat */
  "tplConcat" should "correctly concat n Tuples" in {
    val tup1 = Seq(RFixnum(1), RFixnum(2))
    val tup2 = Seq(RFixnum(3))
    val tup3 = Seq(RFixnum(4), RFixnum(5), RFixnum(6))
    val tups = Seq(Tuple(tup1), Tuple(tup2), Tuple(tup3))

    val result = Seq(RFixnum(1), RFixnum(2), RFixnum(3), RFixnum(4), RFixnum(5), RFixnum(6))

    val newCtxt =
      ctxt.copy(nargs = 3, argvec = Tuple(tups))

    tplConcat.fn(newCtxt) should be(Right(Tuple(result)))
  }

  "tplConcat" should "correctly concat 1 Tuple" in {
    val tup1 = Seq(RFixnum(1))
    val tups = Seq(Tuple(tup1))

    val newCtxt =
      ctxt.copy(nargs = 1, argvec = Tuple(tups))

    tplConcat.fn(newCtxt) should be(
      Right(Tuple(tup1))
    )
  }

  "tplConcat" should "correctly concat 0 Tuples" in {
    val tups = Seq.empty

    val newCtxt =
      ctxt.copy(nargs = 0, argvec = Tuple(tups))

    tplConcat.fn(newCtxt) should be(
      Right(Tuple(Seq.empty))
    )
  }

  it should "fail for non-tuple arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    tplCons.fn(newCtxt) should be('left)
  }

  /** tuple-safe-nth */
  "tplSafeNth" should "correctly return the nth element of a Tuple" in {
    val tup = Seq(RFixnum(1), RFixnum(2), RFixnum(3), RFixnum(4), RFixnum(5), RFixnum(6))
    val n   = RFixnum(3)

    val newCtxt =
      ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(Tuple(tup)), n))

    tplSafeNth.fn(newCtxt) should be(
      Right(RFixnum(4))
    )
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    tplSafeNth.fn(newCtxt) should be('left)
  }

}
