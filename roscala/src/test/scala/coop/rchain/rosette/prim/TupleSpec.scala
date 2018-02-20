package coop.rchain.rosette.prim

import coop.rchain.rosette.prim.tuple._
import coop.rchain.rosette.{Ctxt, Fixnum, Ob, PC, Tuple}
import org.scalatest._

class TupleSpec extends FlatSpec with Matchers {
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
    monitor = null
  )

  /** tuple-cons */
  "tplCons" should "correctly cons an Ob with a Tuple" in {
    val tup = Seq(Fixnum(2), Fixnum(3), Fixnum(4))
    val newCtxt =
      ctxt.copy(nargs = 2, argvec = Tuple.cons(Fixnum(1), Tuple(Tuple(tup))))
    tplCons.fn(newCtxt) should be(Right(Tuple.cons(Fixnum(1), Tuple(tup))))
  }

  it should "fail for non-tuple arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    tplCons.fn(newCtxt) should be('left)
  }

  /** tuple-cons* */
  "tplConsStar" should "correctly cons n Obs with a Tuple" in {
    val tup = Seq(Fixnum(4), Fixnum(5), Fixnum(6))

    val newCtxt =
      ctxt.copy(
        nargs = 4,
        argvec =
          Tuple.cons(Fixnum(1), Tuple.cons(Fixnum(2), Tuple.cons(Fixnum(3), Tuple(Tuple(tup))))))

    tplConsStar.fn(newCtxt) should be(
      Right(Tuple.cons(Fixnum(1), Tuple.cons(Fixnum(2), Tuple.cons(Fixnum(3), Tuple(tup))))))
  }

  "tplConsStar" should "correctly cons 0 Obs with a Tuple" in {
    val tup = Seq(Fixnum(4), Fixnum(5), Fixnum(6))

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
    val tup = Seq(Fixnum(1), Fixnum(2), Fixnum(3))
    val newCtxt =
      ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(Tuple(tup)), Fixnum(4)))
    tplRcons.fn(newCtxt) should be(Right(Tuple.rcons(Tuple(tup), Fixnum(4))))
  }

  it should "fail for non-tuple arguments" in {
    val newCtxt = ctxt.copy(nargs = 2, argvec = Tuple(2, Ob.NIV))
    tplRcons.fn(newCtxt) should be('left)
  }

  /** tuple-concat */
  "tplConcat" should "correctly concat n Tuples" in {
    val tup1 = Seq(Fixnum(1), Fixnum(2))
    val tup2 = Seq(Fixnum(3))
    val tup3 = Seq(Fixnum(4), Fixnum(5), Fixnum(6))
    val tups = Seq(Tuple(tup1), Tuple(tup2), Tuple(tup3))

    val result = Seq(Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))

    val newCtxt =
      ctxt.copy(nargs = 3, argvec = Tuple(tups))

    tplConcat.fn(newCtxt) should be(Right(Tuple(result)))
  }

  "tplConcat" should "correctly concat 1 Tuple" in {
    val tup1 = Seq(Fixnum(1))
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
    val tup = Seq(Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val n   = Fixnum(3)

    val newCtxt =
      ctxt.copy(nargs = 2, argvec = Tuple.rcons(Tuple(Tuple(tup)), n))

    tplSafeNth.fn(newCtxt) should be(
      Right(Fixnum(4))
    )
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    tplSafeNth.fn(newCtxt) should be('left)
  }

  /** tuple-xchg */
  "tplXchg" should "correctly exchange elements of a Tuple" in {
    val tup    = Seq(Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val result = Seq(Fixnum(1), Fixnum(4), Fixnum(3), Fixnum(2), Fixnum(5), Fixnum(6))

    val newCtxt =
      ctxt.copy(nargs = 3,
                argvec = Tuple.rcons(Tuple.rcons(Tuple(Tuple(tup)), Fixnum(1)), Fixnum(3)))

    tplXchg.fn(newCtxt) should be(Right(Tuple(result)))
  }

  it should "fail for out of bounds arguments" in {
    val tup = Seq(Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val newCtxt =
      ctxt.copy(nargs = 3,
                argvec = Tuple.rcons(Tuple.rcons(Tuple(Tuple(tup)), Fixnum(-100)), Fixnum(100)))
    tplXchg.fn(newCtxt) should be('left)
  }

  it should "fail for invalid arguments" in {
    val newCtxt = ctxt.copy(nargs = 5, argvec = Tuple(5, Ob.NIV))
    tplXchg.fn(newCtxt) should be('left)
  }

}
