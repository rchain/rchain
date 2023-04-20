package coop.rchain.roscala.prim

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.ob._
import coop.rchain.roscala.prim.tuple._
import org.scalatest._

class TupleSpec extends FlatSpec with Matchers {
  val globalEnv = new GlobalEnv()
  val ctxt = new Ctxt(
    tag = null,
    nargs = 1,
    pc = 0,
    argvec = Tuple(Fixnum(1)),
    env = null,
    code = null,
    ctxt = null,
    self2 = null,
    selfEnv = null,
    rcvr = null,
    monitor = null
  )

  ctxt.rslt = null
  ctxt.trgt = null

  def reAssignCtxtArgs(ctxt: Ctxt, nargs: Int, args: Tuple): Ctxt = {
    val newCtxt = ctxt.clone()
    newCtxt.nargs = nargs
    newCtxt.argvec = args
    newCtxt
  }

  /** tuple-cons */
  "tplCons" should "correctly cons an Ob with a Tuple" in {
    val tup: Array[Ob] = Array(Fixnum(2), Fixnum(3), Fixnum(4))
    val newCtxt        = reAssignCtxtArgs(ctxt, 2, Tuple(Fixnum(1), Tuple(tup)))
    val ret            = tplCons.fnSimple(newCtxt)
    ret.isRight should be(true)
    ret.right.get.value should be(Array(Fixnum(1)) ++ tup)
  }

  it should "fail for non-tuple arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    tplCons.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** tuple-cons* */
  "tplConsStar" should "correctly cons n Obs with a Tuple" in {
    val tup: Array[Ob] = Array(Fixnum(4), Fixnum(5), Fixnum(6))

    val newCtxt = reAssignCtxtArgs(ctxt, 4, Tuple(Fixnum(1), Fixnum(2), Fixnum(3), Tuple(tup)))

    val ret = tplConsStar.fnSimple(newCtxt)
    ret.isRight should be(true)
    ret.right.get.value should be(
      Tuple.cons(Fixnum(1), Tuple.cons(Fixnum(2), Tuple.cons(Fixnum(3), Tuple(tup)))).value
    )
  }

  "tplConsStar" should "correctly cons 0 Obs with a Tuple" in {
    val tup: Array[Ob] = Array(Fixnum(4), Fixnum(5), Fixnum(6))

    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Tuple(tup)))

    val value = tplConsStar.fnSimple(newCtxt)
    value.isRight should be(true)
    value.right.get.value should be(tup)
  }

  it should "fail for non-tuple arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    tplCons.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** tuple-rcons */
  "tplRcons" should "correctly rcons an Ob with a Tuple" in {
    val tup: Array[Ob] = Array(Fixnum(1), Fixnum(2), Fixnum(3))
    val newCtxt        = reAssignCtxtArgs(ctxt, 2, Tuple.rcons(Tuple(Tuple(tup)), Fixnum(4)))
    val value          = tplRcons.fnSimple(newCtxt)
    value.isRight should be(true)
    value.right.get.value should be(tup :+ Fixnum(4))
  }

  it should "fail for non-tuple arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(2, Niv))
    tplRcons.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** tuple-concat */
  "tplConcat" should "correctly concat n Tuples" in {
    val tup1 = Array[Ob](Fixnum(1), Fixnum(2))
    val tup2 = Array[Ob](Fixnum(3))
    val tup3 = Array[Ob](Fixnum(4), Fixnum(5), Fixnum(6))
    val tups = Array[Ob](Tuple(tup1), Tuple(tup2), Tuple(tup3))

    val result: Array[Ob] = Array(Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))

    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(tups))

    val value = tplConcat.fnSimple(newCtxt)
    value.isRight should be(true)
    value.right.get.value should be(result)
  }

  "tplConcat" should "correctly concat 1 Tuple" in {
    val tup1 = Array[Ob](Fixnum(1))
    val tups = Array[Ob](Tuple(tup1))

    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(tups))

    val value = tplConcat.fnSimple(newCtxt)
    value.isRight should be(true)
    value.right.get.value should be(tup1)
  }

  "tplConcat" should "correctly concat 0 Tuples" in {
    val tups = Array.empty[Ob]

    val newCtxt = reAssignCtxtArgs(ctxt, 0, Tuple(tups))

    val value = tplConcat.fnSimple(newCtxt)
    value.isRight should be(true)
    value.right.get.value should be(Array.empty)
  }

  it should "fail for non-tuple arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    tplCons.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** tuple-safe-nth */
  "tplSafeNth" should "correctly return the nth element of a Tuple" in {
    val tup = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val n   = Fixnum(3)

    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple.rcons(Tuple(Tuple(tup)), n))

    tplSafeNth.fnSimple(newCtxt) should be(
      Right(Fixnum(4))
    )
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    tplSafeNth.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** tuple-xchg */
  "tplXchg" should "correctly exchange elements of a Tuple" in {
    val tup    = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val result = Array[Ob](Fixnum(1), Fixnum(4), Fixnum(3), Fixnum(2), Fixnum(5), Fixnum(6))

    val newCtxt =
      reAssignCtxtArgs(ctxt, 3, Tuple.rcons(Tuple.rcons(Tuple(Tuple(tup)), Fixnum(1)), Fixnum(3)))

    val ret = tplXchg.fnSimple(newCtxt)
    ret.isRight should be(true)
    ret.right.get.value should be(result)
  }

  it should "fail for out of bounds arguments" in {
    val tup = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val newCtxt =
      reAssignCtxtArgs(
        ctxt,
        3,
        Tuple.rcons(Tuple.rcons(Tuple(Tuple(tup)), Fixnum(-100)), Fixnum(100))
      )
    tplXchg.fnSimple(newCtxt) should be(Symbol("left"))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    tplXchg.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** tuple-head */
  "tplHead" should "correctly return the 0th element of a Tuple" in {
    val tup = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))

    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Tuple(tup)))

    tplHead.fnSimple(newCtxt) should be(
      Right(Fixnum(1))
    )
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    tplHead.fnSimple(newCtxt) should be(Symbol("left"))
  }

  it should "return NIL for an empty Tuple" in {
    val tup     = Array[Ob]()
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Tuple(tup)))
    tplHead.fnSimple(newCtxt) should be(Right(Nil))
  }

  /** tuple-last */
  "tplLast" should "correctly return the last element of a Tuple" in {
    val tup = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))

    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Tuple(tup)))

    tplLast.fnSimple(newCtxt) should be(
      Right(Fixnum(6))
    )
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    tplLast.fnSimple(newCtxt) should be(Symbol("left"))
  }

  it should "return NIL for an empty Tuple" in {
    val tup     = Array[Ob]()
    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Tuple(tup)))
    tplLast.fnSimple(newCtxt) should be(Right(Nil))
  }

  /** tuple-tail */
  "tplTail" should "correctly return the 2nd through last element of a Tuple" in {
    val tup  = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val tail = Array[Ob](Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))

    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(Tuple(tup)))

    val ret = tplTail.fnSimple(newCtxt)
    ret.isRight should be(true)
    ret.right.get.value should be(tail)
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    tplTail.fnSimple(newCtxt) should be(Symbol("left"))
  }

  it should "return empty Tuple for an empty Tuple input" in {
    val empty: Array[Ob] = Array.empty
    val newCtxt          = reAssignCtxtArgs(ctxt, 1, Tuple(Tuple(empty)))
    tplTail.fnSimple(newCtxt) should be(Right(Nil))
  }

  it should "return empty Tuple for a single element Tuple input" in {
    val tup              = Array[Ob](Fixnum(1))
    val empty: Array[Ob] = Array.empty
    val newCtxt          = reAssignCtxtArgs(ctxt, 1, Tuple(Tuple(tup)))
    val ret              = tplTail.fnSimple(newCtxt)
    ret.isRight should be(true)

    ret.right.get.value should be(empty)
  }

  /** tuple-new */
  "tplNew" should "correctly create a new Tuple with n Obs" in {
    val tup = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))

    val newCtxt = reAssignCtxtArgs(ctxt, 7, Tuple.cons(Fixnum(1), Tuple(tup)))

    val ret = tplNew.fnSimple(newCtxt)
    ret.isRight should be(true)
    ret.right.get.value should be(tup)
  }

  it should "correctly create a new Tuple with 0 Obs" in {
    val empty: Array[Ob] = Array.empty

    val newCtxt = reAssignCtxtArgs(ctxt, 1, Tuple(empty))

    val ret = tplNew.fnSimple(newCtxt)
    ret.isRight should be(true)
    ret.right.get.value should be(empty)
  }

  /** tuple-new-n */
  "tplNewN" should "correctly create a new Tuple with n duplicate Obs" in {
    val tup     = Tuple(6, Fixnum(1))
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(Array[Ob](Fixnum(0), Fixnum(6), Fixnum(1))))
    val ret     = tplNewN.fnSimple(newCtxt)
    ret.isRight should be(true)
    ret.right.get.value should be(tup.value)
  }

  it should "return NIL for n<=0" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 3, Tuple(Array[Ob](Fixnum(0), Fixnum(0), Fixnum(1))))

    tplNewN.fnSimple(newCtxt) should be(Right(Nil))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    tplNewN.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** tuple-mem? */
  "tplMemQ" should "return true if the Ob exists in the Tuple" in {
    val tup     = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple.rcons(Tuple(Tuple(tup)), Fixnum(4)))
    tplMemQ.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the Ob is not in the Tuple" in {
    val tup     = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple.rcons(Tuple(Tuple(tup)), Fixnum(17)))

    tplMemQ.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return false for an empty Tuple" in {
    val tup: Array[Ob] = Array.empty
    val newCtxt        = reAssignCtxtArgs(ctxt, 2, Tuple.rcons(Tuple(Tuple(tup)), Fixnum(17)))

    val ret = tplMemQ.fnSimple(newCtxt)
    ret should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    tplMemQ.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** tuple-matches? */
  "tplMatchesP" should "return true if the Tuple matches the pattern" in {
    val pat  = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3))
    val tup  = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val tups = Array[Ob](Tuple(pat), Tuple(tup))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(tups))
    tplMatchesP.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the Tuple does not match the pattern" in {
    val pat  = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val tup  = Array[Ob](Fixnum(1), Fixnum(2), Fixnum(3))
    val tups = Array[Ob](Tuple(pat), Tuple(tup))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(tups))
    tplMatchesP.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return false for an empty Tuple" in {
    val pat: Array[Ob]  = Array(Fixnum(1), Fixnum(2), Fixnum(3), Fixnum(4), Fixnum(5), Fixnum(6))
    val tup: Array[Ob]  = Array.empty
    val tups: Array[Ob] = Array(Tuple(pat), Tuple(tup))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(tups))
    tplMatchesP.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 5, Tuple(5, Niv))
    tplMemQ.fnSimple(newCtxt) should be(Symbol("left"))
  }

}
