package coop.rchain.rosette.prim

import coop.rchain.roscala.ob._
import coop.rchain.roscala.prim.rblstring._
import org.scalatest._

class RblStringSpec extends FlatSpec with Matchers {
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

  ctxt.trgt = null
  ctxt.rslt = null

  def reAssignCtxtArgs(ctxt: Ctxt, nargs: Int, args: Tuple): Ctxt = {
    val newCtxt = ctxt.clone()
    newCtxt.nargs = nargs
    newCtxt.argvec = args
    newCtxt
  }

  /** Case Sensistive compares */
  /** string= */
  "stringEq" should "return true if the strings match" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings do not match" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringEq.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string!= */
  "stringNEq" should "return true if the strings do not match" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringNEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings do match" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringNEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringNEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringNEq.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string< */
  "stringLess" should "return true if the left string is less than the right one" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringLess.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringLess.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return false if the right string is less than the left one" in {
    val s1   = "ghijkl"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringLess.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringLess.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringLess.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string<= */
  "stringLEq" should "return true if the left string is less than the right one" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringLEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return true if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringLEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the right string is less than the left one" in {
    val s1   = "ghijkl"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringLEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringLEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringLEq.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string> */
  "stringGtr" should "return true if the left string is greater than the right one" in {
    val s1   = "ghijkl"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringGtr.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringGtr.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return false if the right string is greater than the left one" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringGtr.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringGtr.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringGtr.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string>= */
  "stringGEq" should "return true if the left string is greater than the right one" in {
    val s1   = "ghijkl"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringGEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return true if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringGEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the right string is greater than the left one" in {
    val s1   = "abcdef"
    val s2   = "ghijkl"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringGEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringGEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringGEq.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** Case Insensitive compares */
  /** string-ci= */
  "stringCiEq" should "return true if the strings match" in {
    val s1   = "abcdef"
    val s2   = "aBcDeF"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings do not match" in {
    val s1   = "aBcDeF"
    val s2   = "ghijkl"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringCiEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringCiEq.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-ci!= */
  "stringCiNEq" should "return true if the strings do not match" in {
    val s1   = "AbCdEf"
    val s2   = "ghijkl"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiNEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings do match" in {
    val s1   = "AbCdEf"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiNEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringCiNEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringCiNEq.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-ci< */
  "stringCiLess" should "return true if the left string is less than the right one" in {
    val s1   = "abcdef"
    val s2   = "GhIjKl"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiLess.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "AbCdEf"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiLess.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return false if the right string is less than the left one" in {
    val s1   = "GhIjKl"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiLess.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringCiLess.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringCiLess.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-ci<= */
  "stringCiLEq" should "return true if the left string is less than the right one" in {
    val s1   = "AbCdEf"
    val s2   = "ghijkl"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiLEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return true if the strings are equal" in {
    val s1   = "abcdef"
    val s2   = "aBcDeF"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiLEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the right string is less than the left one" in {
    val s1   = "GhIjKl"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiLEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringCiLEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringCiLEq.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-ci> */
  "stringCiGtr" should "return true if the left string is greater than the right one" in {
    val s1   = "gHiJkL"
    val s2   = "abcdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiGtr.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the strings are equal" in {
    val s1   = "AbCdEf"
    val s2   = "aBcDeF"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiGtr.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return false if the right string is greater than the left one" in {
    val s1   = "AbCdEf"
    val s2   = "gHiJkL"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiGtr.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringCiGtr.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringCiGtr.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-ci>= */
  "stringCiGEq" should "return true if the left string is greater than the right one" in {
    val s1   = "GHIjkl"
    val s2   = "abcDEF"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiGEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return true if the strings are equal" in {
    val s1   = "abcDEF"
    val s2   = "ABCdef"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiGEq.fnSimple(newCtxt) should be(Right(RblBool(true)))
  }

  it should "return false if the right string is greater than the left one" in {
    val s1   = "aBCDef"
    val s2   = "ghIJKl"
    val strs = Tuple(RblString(s1), RblString(s2))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)
    stringCiGEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "return RblBool(false) for invalid right side argument" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(RblString("foo"), Fixnum(42)))
    stringCiGEq.fnSimple(newCtxt) should be(Right(RblBool(false)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringCiGEq.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-concat */
  "string-concat" should "correctly concatenate n strings" in {
    val s1   = "abcd"
    val s2   = "ef"
    val s3   = "ghi"
    val strs = Tuple(RblString(s1), RblString(s2), RblString(s3))
    val res  = s1 + s2 + s3

    val newCtxt = reAssignCtxtArgs(ctxt, strs.value.length, strs)

    stringConcat.fnSimple(newCtxt) should be(Right(RblString(res)))
  }

  it should "return an empty string with no input strings" in {
    val strs = Nil
    val res  = ""

    val newCtxt = reAssignCtxtArgs(ctxt, strs.value.length, strs)

    stringConcat.fnSimple(newCtxt) should be(Right(RblString(res)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringConcat.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-join */
  "string-join" should "correctly join n strings" in {
    val s1   = "abcd"
    val s2   = "ef"
    val s3   = "ghi"
    val strs = Tuple(RblString(s1), RblString(s2), RblString(s3))
    val sep  = ":"

    val parmsNeither = Tuple.cons(Fixnum(0), Tuple.cons(RblString(sep), Tuple(strs)))
    val parmsFront   = Tuple.cons(Fixnum(1), Tuple.cons(RblString(sep), Tuple(strs)))
    val parmsRear    = Tuple.cons(Fixnum(2), Tuple.cons(RblString(sep), Tuple(strs)))
    val parmsBoth    = Tuple.cons(Fixnum(3), Tuple.cons(RblString(sep), Tuple(strs)))

    val ctxtNeither = reAssignCtxtArgs(ctxt, 2, parmsNeither)
    val ctxtFront   = reAssignCtxtArgs(ctxt, 2, parmsFront)
    val ctxtRear    = reAssignCtxtArgs(ctxt, 2, parmsRear)
    val ctxtBoth    = reAssignCtxtArgs(ctxt, 2, parmsBoth)

    val res = s1 + sep + s2 + sep + s3

    stringJoin.fnSimple(ctxtNeither) should be(Right(RblString(res)))
    stringJoin.fnSimple(ctxtFront) should be(Right(RblString(sep + res)))
    stringJoin.fnSimple(ctxtRear) should be(Right(RblString(res + sep)))
    stringJoin.fnSimple(ctxtBoth) should be(Right(RblString(sep + res + sep)))
  }

  it should "return an empty string for an empty Tuple" in {
    val strs      = Nil
    val sep       = ":"
    val parmsBoth = Tuple.cons(Fixnum(3), Tuple.cons(RblString(sep), Tuple(strs)))
    val res       = ""

    val ctxtBoth = reAssignCtxtArgs(ctxt, 2, parmsBoth)
    stringJoin.fnSimple(ctxtBoth) should be(Right(RblString(res)))
  }

  it should "fail if tuple contains non-RblString objects" in {
    val strs  = Tuple(Fixnum(1), Fixnum(2), Fixnum(3))
    val sep   = ":"
    val parms = Tuple.cons(Fixnum(3), Tuple.cons(RblString(sep), strs))

    val ctxtBoth = reAssignCtxtArgs(ctxt, 2, parms)

    stringJoin.fnSimple(ctxtBoth) should be(Symbol("left"))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringJoin.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-length */
  "string-length" should "return the length of the string" in {
    val s    = "abcdefghi"
    val strs = Tuple(RblString(s))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)

    stringLength.fnSimple(newCtxt) should be(Right(Fixnum(9)))
  }

  it should "return 0 for an empty string" in {
    val s    = ""
    val strs = Tuple(RblString(s))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, strs)

    stringLength.fnSimple(newCtxt) should be(Right(Fixnum(0)))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringJoin.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-set-nth */
  "string-set-nth" should "replace the nth character of the string" in {
    val s     = "abcdefghi"
    val parms = Tuple(RblString(s), Fixnum(2), RblChar('C'))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)

    stringSetNth.fnSimple(newCtxt) should be(Right(RblString("abCdefghi")))

  }

  it should "fail for index out of bounds" in {
    val s     = "abcdefghi"
    val parms = Tuple(RblString(s), Fixnum(20), RblChar('C'))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)

    stringSetNth.fnSimple(newCtxt) should be(Symbol("left"))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringSetNth.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-new */
  "string-new" should "create a string of length n of the specified character" in {
    val parms   = Tuple(Fixnum(5), RblChar('C'))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)

    stringNew.fnSimple(newCtxt) should be(Right(RblString("CCCCC")))
  }

  it should "create a string of n spaces if no character specified" in {
    val parms   = Tuple(Fixnum(5))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)

    stringNew.fnSimple(newCtxt) should be(Right(RblString("     ")))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringNew.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-mem? */
  "string-mem?" should "return #t if the specified character is in the string" in {
    val s     = "abcdefghi"
    val parms = Tuple(RblString(s), RblChar('c'))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)

    stringMemQ.fnSimple(newCtxt) should be(Right(RblBool(true)))

  }

  it should "return #f if the specified character is not in the string" in {
    val s     = "abcdefghi"
    val parms = Tuple(RblString(s), RblChar('z'))

    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)

    stringMemQ.fnSimple(newCtxt) should be(Right(RblBool(false)))

  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringMemQ.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-get-token */
  "string-get-token" should "correctly return the first token" in {
    val sep = ".,-=/*"
    val str = "aZ,bY.cX-dW=eV/fU*gT"

    val parms   = Tuple(RblString(str), Fixnum(0), RblString(sep))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    stringGetToken.fnSimple(newCtxt) should be(Right(RblString("aZ")))
  }

  it should "correctly return the last token" in {
    val sep = ".,-=/*"
    val str = "aZ,bY.cX-dW=eV/fU*gT"

    val parms   = Tuple(RblString(str), Fixnum(6), RblString(sep))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    stringGetToken.fnSimple(newCtxt) should be(Right(RblString("gT")))
  }

  it should "correctly return a token from the middle" in {
    val sep = ".,-=/*"
    val str = "aZ,bY.cX-dW=eV/fU*gT"

    val parms   = Tuple(RblString(str), Fixnum(3), RblString(sep))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    stringGetToken.fnSimple(newCtxt) should be(Right(RblString("dW")))
  }

  it should "return an empty string for index out of bounds" in {
    val sep = ".,-=/*"
    val str = "aZ,bY.cX-dW=eV/fU*gT"

    val parms   = Tuple(RblString(str), Fixnum(8), RblString(sep))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    stringGetToken.fnSimple(newCtxt) should be(Right(RblString("")))
  }

  it should "return an empty string if delimiter not found" in {
    val sep = "&"
    val str = "aZ,bY.cX-dW=eV/fU*gT"

    val parms   = Tuple(RblString(str), Fixnum(8), RblString(sep))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    stringGetToken.fnSimple(newCtxt) should be(Right(RblString("")))
  }

  it should "return an empty string for an empty string" in {
    val sep = "&"
    val str = ""

    val parms   = Tuple(RblString(str), Fixnum(8), RblString(sep))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    stringGetToken.fnSimple(newCtxt) should be(Right(RblString("")))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringMemQ.fnSimple(newCtxt) should be(Symbol("left"))
  }

  /** string-split */
  "string-split" should "correctly tokenize a full string" in {
    val sep = ".,-=/*"
    val str = "aZ,bY.cX-dW=eV/fU*gT"
    val res = Tuple(
      RblString("aZ"),
      RblString("bY"),
      RblString("cX"),
      RblString("dW"),
      RblString("eV"),
      RblString("fU"),
      RblString("gT"),
      Niv
    )

    val parms   = Tuple(RblString(str), RblString(sep), Fixnum(33))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    stringSplit.fnSimple(newCtxt).right.get.value should be(res.value)
  }

  it should "correctly tokenize a full string with unspecified count" in {
    val sep = ".,-=/*"
    val str = "aZ,bY.cX-dW=eV/fU*gT"
    val res = Tuple(
      RblString("aZ"),
      RblString("bY"),
      RblString("cX"),
      RblString("dW"),
      RblString("eV"),
      RblString("fU"),
      RblString("gT"),
      Niv
    )

    val parms   = Tuple(RblString(str), RblString(sep))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    val ret     = stringSplit.fnSimple(newCtxt)
    ret.isRight should be(true)
    ret.right.get.value should be(res.value)
  }

  it should "correctly ignore leading and trailing separators" in {
    val sep = ".,-=/*"
    val str = ",,aZ,bY.cX-dW=eV/fU*gT**"
    val res = Tuple(
      RblString("aZ"),
      RblString("bY"),
      RblString("cX"),
      RblString("dW"),
      RblString("eV"),
      RblString("fU"),
      RblString("gT"),
      Niv
    )

    val parms   = Tuple(RblString(str), RblString(sep))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)

    val ret = stringSplit.fnSimple(newCtxt)
    ret.isRight should be(true)
    ret.right.get.value should be(res.value)
  }

  it should "return an empty Tuple with a count of zero" in {
    val sep = ".,-=/*"
    val str = "aZ,bY.cX-dW=eV/fU*gT"

    val parms   = Tuple(RblString(str), RblString(sep), Fixnum(0))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    stringSplit.fnSimple(newCtxt) should be(Right(Nil))
  }

  it should "return an empty Tuple with a count less than zero" in {
    val sep = ".,-=/*"
    val str = "aZ,bY.cX-dW=eV/fU*gT"

    val parms   = Tuple(RblString(str), RblString(sep), Fixnum(-1))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    stringSplit.fnSimple(newCtxt) should be(Right(Nil))
  }

  it should "return a Tuple of chars if no separators" in {
    val sep = ""
    val str = "ABC"
    val res = Tuple(RblChar('A'), RblChar('B'), RblChar('C'))

    val parms   = Tuple(RblString(str), RblString(sep))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    val ret     = stringSplit.fnSimple(newCtxt)
    ret.isRight should be(true)
    ret.right.get.value should be(res.value)
  }

  it should "correctly return a Tuple of selected tokens and the remaining string" in {
    val sep = ".,-=/*"
    val str = "aZ,bY.cX-dW=eV/fU*gT"
    val res = Tuple(RblString("aZ"), RblString("bY"), RblString("cX-dW=eV/fU*gT"))

    val parms   = Tuple(RblString(str), RblString(sep), Fixnum(2))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)

    val ret = stringSplit.fnSimple(newCtxt)
    ret.isRight should be(true)
    ret.right.get.value should be(res.value)
  }

  it should "fail for invalid Count type" in {
    val sep = ".,-=/*"
    val str = "aZ,bY.cX-dW=eV/fU*gT"

    val parms   = Tuple(RblString(str), RblString(sep), RblString("foo"))
    val newCtxt = reAssignCtxtArgs(ctxt, 2, parms)
    stringSplit.fnSimple(newCtxt) should be(Symbol("left"))
  }

  it should "fail for invalid arguments" in {
    val newCtxt = reAssignCtxtArgs(ctxt, 2, Tuple(5, Niv))
    stringSplit.fnSimple(newCtxt) should be(Symbol("left"))
  }

}
